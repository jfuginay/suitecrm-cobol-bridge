#!/bin/bash

# SuiteCRM COBOL Bridge - Setup Validation Script
# This script checks that all services are properly configured and running

echo "🔍 SuiteCRM COBOL Bridge - Setup Validation"
echo "=========================================="

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track validation status
ERRORS=0
WARNINGS=0

# Function to check if a command exists
check_command() {
    if command -v $1 &> /dev/null; then
        echo -e "${GREEN}✓${NC} $1 is installed"
    else
        echo -e "${RED}✗${NC} $1 is not installed"
        ERRORS=$((ERRORS + 1))
    fi
}

# Function to check if a port is available
check_port() {
    if lsof -i :$1 &> /dev/null; then
        echo -e "${RED}✗${NC} Port $1 is in use"
        ERRORS=$((ERRORS + 1))
    else
        echo -e "${GREEN}✓${NC} Port $1 is available"
    fi
}

# Function to check if a service is running
check_service() {
    if docker ps | grep -q $1; then
        echo -e "${GREEN}✓${NC} $1 is running"
    else
        echo -e "${RED}✗${NC} $1 is not running"
        ERRORS=$((ERRORS + 1))
    fi
}

# Function to check if a URL is accessible
check_url() {
    if curl -s -o /dev/null -w "%{http_code}" $1 | grep -q "200\|301\|302"; then
        echo -e "${GREEN}✓${NC} $1 is accessible"
    else
        echo -e "${YELLOW}⚠${NC} $1 is not accessible yet"
        WARNINGS=$((WARNINGS + 1))
    fi
}

echo ""
echo "1. Checking Prerequisites..."
echo "----------------------------"
check_command docker
check_command docker-compose
check_command git
check_command curl

echo ""
echo "2. Checking Required Ports..."
echo "-----------------------------"
PORTS=(80 3000 3001 3003 3005 3006 8080 8081 8082 8083 9090 6379 3306)
for port in "${PORTS[@]}"; do
    check_port $port
done

echo ""
echo "3. Checking Docker Services..."
echo "------------------------------"
if docker-compose ps &> /dev/null; then
    SERVICES=(suitecrm mysql redis api-gateway monitoring rules-editor mobile-api debugger cloud-burst)
    for service in "${SERVICES[@]}"; do
        check_service $service
    done
else
    echo -e "${YELLOW}⚠${NC} Docker Compose is not running. Run ./start.sh first"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""
echo "4. Checking Service Endpoints..."
echo "--------------------------------"
if docker-compose ps | grep -q "Up"; then
    sleep 5  # Give services time to fully start
    check_url "http://localhost:8080"      # SuiteCRM
    check_url "http://localhost:8081"      # Monitoring
    check_url "http://localhost:8082"      # Rules Editor
    check_url "http://localhost:8083"      # Debugger
    check_url "http://localhost:3000/health" # API Gateway
    check_url "http://localhost:3001"      # Mobile API
    check_url "http://localhost:3003"      # Cloud Burst
    check_url "http://localhost:3006"      # Grafana
fi

echo ""
echo "5. Checking File Structure..."
echo "-----------------------------"
REQUIRED_DIRS=(
    "cobol-core"
    "api-gateway"
    "monitoring"
    "business-rules"
    "mobile-api"
    "debugger"
    "cloud-burst"
    "suitecrm-integration"
    "demo"
    "docs"
)

for dir in "${REQUIRED_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo -e "${GREEN}✓${NC} Directory $dir exists"
    else
        echo -e "${RED}✗${NC} Directory $dir is missing"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""
echo "6. Checking COBOL Programs..."
echo "-----------------------------"
COBOL_PROGRAMS=(
    "cobol-core/enhanced-financial-calc.cob"
    "suitecrm-integration/samples/credit_calculator.cob"
)

for program in "${COBOL_PROGRAMS[@]}"; do
    if [ -f "$program" ]; then
        echo -e "${GREEN}✓${NC} COBOL program $program exists"
    else
        echo -e "${RED}✗${NC} COBOL program $program is missing"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""
echo "7. Checking Environment..."
echo "-------------------------"
if [ -f ".env" ]; then
    echo -e "${GREEN}✓${NC} .env file exists"
else
    echo -e "${YELLOW}⚠${NC} .env file is missing (will be created by start.sh)"
    WARNINGS=$((WARNINGS + 1))
fi

# Check Docker resources
DOCKER_MEM=$(docker system info 2>/dev/null | grep "Total Memory" | awk '{print $3}' | sed 's/GiB//')
if [ ! -z "$DOCKER_MEM" ]; then
    if (( $(echo "$DOCKER_MEM < 8" | bc -l) )); then
        echo -e "${YELLOW}⚠${NC} Docker has less than 8GB memory allocated (found: ${DOCKER_MEM}GB)"
        WARNINGS=$((WARNINGS + 1))
    else
        echo -e "${GREEN}✓${NC} Docker has sufficient memory (${DOCKER_MEM}GB)"
    fi
fi

echo ""
echo "=========================================="
echo "Validation Summary:"
echo "=========================================="

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ All checks passed! The system is ready.${NC}"
    echo ""
    echo "Next steps:"
    echo "1. Run './start.sh' if services are not running"
    echo "2. Access services at the URLs shown above"
    echo "3. Run the demo: docker exec cobol-api-gateway node /app/demo/run-all-features.js"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠️  Setup has $WARNINGS warnings but should work.${NC}"
    echo ""
    echo "The warnings above are typically resolved by:"
    echo "1. Running './start.sh' to start services"
    echo "2. Waiting 30-60 seconds for services to initialize"
    echo "3. Increasing Docker memory allocation if needed"
    exit 0
else
    echo -e "${RED}❌ Setup has $ERRORS errors and $WARNINGS warnings.${NC}"
    echo ""
    echo "Please fix the errors above before proceeding:"
    echo "1. Install missing prerequisites"
    echo "2. Free up required ports"
    echo "3. Check Docker is running"
    exit 1
fi