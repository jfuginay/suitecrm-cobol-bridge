#!/bin/bash

# Health check script for COBOL worker

# Check if worker process is running
if ! pgrep -f "node.*cobol-worker" > /dev/null; then
    echo "Worker process not running"
    exit 1
fi

# Check if worker HTTP endpoint is responding
if ! curl -f -s -o /dev/null http://localhost:8080/health; then
    echo "Worker HTTP endpoint not responding"
    exit 1
fi

# Check COBOL runtime
if ! command -v cobc > /dev/null; then
    echo "COBOL compiler not available"
    exit 1
fi

# Check DB2 client (if configured)
if [ ! -z "$DB2_HOST" ]; then
    if ! command -v db2 > /dev/null; then
        echo "DB2 client not available"
        exit 1
    fi
fi

# Check disk space
DISK_USAGE=$(df /var/cobol | tail -1 | awk '{print $5}' | sed 's/%//')
if [ $DISK_USAGE -gt 90 ]; then
    echo "Disk usage critical: ${DISK_USAGE}%"
    exit 1
fi

echo "Health check passed"
exit 0