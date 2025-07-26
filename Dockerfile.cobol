FROM ubuntu:22.04

# Metadata
LABEL maintainer="SuiteCRM COBOL Bridge Team"
LABEL description="Enhanced GnuCOBOL runtime with monitoring and security"
LABEL version="2.0"

# Install GnuCOBOL and dependencies with security tools
RUN apt-get update && apt-get install -y \
    gnucobol \
    libcob-dev \
    build-essential \
    wget \
    curl \
    git \
    python3 \
    python3-pip \
    python3-dev \
    # Security and monitoring tools\
    apparmor \
    apparmor-utils \
    seccomp \
    libseccomp-dev \
    libcap2-bin \
    strace \
    ltrace \
    # System monitoring\
    sysstat \
    htop \
    iotop \
    # Required libraries\
    libdb-dev \
    libncurses5-dev \
    libgmp-dev \
    libxml2-dev \
    uuid-runtime \
    && rm -rf /var/lib/apt/lists/*

# Create user for COBOL execution (non-root security)
RUN groupadd -g 1001 cobol && \
    useradd -u 1001 -g cobol -m -s /bin/bash cobol

# Create directories with proper permissions
RUN mkdir -p /cobol-programs /compiled /scripts /logs /tmp/cobol-exec && \
    chown -R cobol:cobol /cobol-programs /compiled /scripts /logs /tmp/cobol-exec && \
    chmod 755 /cobol-programs /compiled /scripts && \
    chmod 1777 /tmp/cobol-exec

# Copy COBOL programs
COPY --chown=cobol:cobol cobol-core/*.cob /cobol-programs/

# Copy scripts
COPY --chown=cobol:cobol scripts/compile-all.sh /scripts/
COPY --chown=cobol:cobol scripts/cobol-wrapper.py /scripts/

# Copy security profiles
COPY --chown=root:root docker/security/apparmor-cobol /etc/apparmor.d/cobol-runtime
COPY --chown=root:root docker/security/seccomp-cobol.json /etc/seccomp/cobol-runtime.json

# Make scripts executable
RUN chmod +x /scripts/compile-all.sh /scripts/cobol-wrapper.py

# Switch to cobol user for compilation
USER cobol
WORKDIR /cobol-programs

# Compile all COBOL programs with monitoring support
RUN /scripts/compile-all.sh

# Switch back to root for Python package installation
USER root

# Install Python dependencies for COBOL wrapper with specific versions
RUN pip3 install --no-cache-dir \
    flask==2.3.3 \
    flask-cors==4.0.0 \
    requests==2.31.0 \
    psutil==5.9.5 \
    prometheus-client==0.17.1 \
    gunicorn==21.2.0

# Create monitoring configuration
RUN echo '{
  "monitoring": {
    "enabled": true,
    "interval": 1000,
    "metrics": ["cpu", "memory", "io", "execution_time"]
  },
  "security": {
    "sandbox": true,
    "max_memory": "512M",
    "max_cpu": "1",
    "allowed_syscalls": "default"
  },
  "debug": {
    "enabled": false,
    "trace_level": "info"
  }
}' > /etc/cobol-runtime.json

# Create healthcheck script
RUN echo '#!/bin/bash
curl -f http://localhost:5000/health || exit 1' > /scripts/healthcheck.sh && \
    chmod +x /scripts/healthcheck.sh

# Expose ports for COBOL execution service and monitoring
EXPOSE 5000 5001

# Add healthcheck
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD /scripts/healthcheck.sh

# Environment variables
ENV COBOL_BIN_PATH=/compiled \
    TEMP_PATH=/tmp/cobol-exec \
    MAX_EXECUTION_TIME=300 \
    MAX_FILE_SIZE=104857600 \
    PYTHONUNBUFFERED=1 \
    COB_LIBRARY_PATH=/usr/lib/gnucobol \
    COB_CONFIG_DIR=/usr/share/gnucobol/config \
    COB_RUNTIME_CONFIG=default.conf

# Security capabilities
RUN setcap 'cap_setuid,cap_setgid+ep' /usr/bin/python3

# Create startup script with security enforcement
RUN echo '#!/bin/bash
set -e

# Load AppArmor profile if available
if [ -f /etc/apparmor.d/cobol-runtime ] && command -v aa-enforce &> /dev/null; then
    aa-enforce /etc/apparmor.d/cobol-runtime || true
fi

# Create required directories
mkdir -p /tmp/cobol-exec /logs
chown cobol:cobol /tmp/cobol-exec /logs

# Start monitoring agent in background
if [ "${ENABLE_MONITORING}" = "true" ]; then
    python3 /scripts/monitor-agent.py &
fi

# Drop privileges and run wrapper service
exec su-exec cobol:cobol python3 /scripts/cobol-wrapper.py
' > /scripts/startup.sh && chmod +x /scripts/startup.sh

# Install su-exec for privilege dropping
RUN apt-get update && apt-get install -y su-exec && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /compiled

# Run as non-root user by default
USER cobol

# Start COBOL execution service with monitoring
CMD ["/scripts/startup.sh"]