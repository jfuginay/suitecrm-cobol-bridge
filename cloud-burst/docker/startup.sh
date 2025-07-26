#!/bin/bash
set -e

echo "Starting COBOL Worker Container..."

# Set up environment
export NODE_ENV=${NODE_ENV:-production}
export WORKER_ID=${HOSTNAME}

# Wait for mainframe connectivity (optional)
if [ ! -z "$MAINFRAME_HOST" ]; then
    echo "Testing mainframe connectivity..."
    timeout 30 bash -c "until nc -z $MAINFRAME_HOST 22; do sleep 1; done" || {
        echo "Warning: Cannot reach mainframe SSH port"
    }
fi

# Wait for DB2 connectivity (optional)
if [ ! -z "$DB2_HOST" ] && [ ! -z "$DB2_PORT" ]; then
    echo "Testing DB2 connectivity..."
    timeout 30 bash -c "until nc -z $DB2_HOST $DB2_PORT; do sleep 1; done" || {
        echo "Warning: Cannot reach DB2 port"
    }
fi

# Initialize COBOL environment
echo "Initializing COBOL environment..."
if [ -d "/opt/cobol-utils/init" ]; then
    for script in /opt/cobol-utils/init/*.sh; do
        if [ -r "$script" ]; then
            echo "Running init script: $script"
            . "$script"
        fi
    done
fi

# Create required directories
mkdir -p /var/cobol/programs /var/cobol/data /var/cobol/temp /var/log/cobol

# Set permissions
chmod 755 /var/cobol/programs
chmod 755 /var/cobol/data
chmod 777 /var/cobol/temp

# Start supervisord
echo "Starting supervisor..."
exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf