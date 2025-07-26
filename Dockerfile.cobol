FROM ubuntu:22.04

# Install GnuCOBOL and dependencies
RUN apt-get update && apt-get install -y \
    gnucobol \
    libcob-dev \
    build-essential \
    wget \
    curl \
    git \
    python3 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Create directories
RUN mkdir -p /cobol-programs /compiled /scripts

# Copy COBOL programs
COPY cobol-core/*.cob /cobol-programs/

# Copy compilation script
COPY scripts/compile-all.sh /scripts/

# Make script executable
RUN chmod +x /scripts/compile-all.sh

# Compile all COBOL programs
RUN cd /cobol-programs && \
    for f in *.cob; do \
        echo "Compiling $f..." && \
        cobc -x -o /compiled/${f%.cob} $f || true; \
    done

# Install Python dependencies for COBOL wrapper
RUN pip3 install flask requests

# Copy COBOL execution wrapper
COPY scripts/cobol-wrapper.py /scripts/

# Expose port for COBOL execution service
EXPOSE 5000

# Set working directory
WORKDIR /compiled

# Start COBOL execution service
CMD ["python3", "/scripts/cobol-wrapper.py"]