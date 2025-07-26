#!/bin/bash

# SuiteCRM COBOL Bridge - Quick Start Script

echo "🚀 Starting SuiteCRM COBOL Bridge..."
echo "=================================="

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "❌ Docker is not installed. Please install Docker first."
    echo "Visit: https://docs.docker.com/get-docker/"
    exit 1
fi

# Check if Docker Compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo "❌ Docker Compose is not installed. Please install Docker Compose first."
    echo "Visit: https://docs.docker.com/compose/install/"
    exit 1
fi

# Create necessary directories
echo "📁 Creating directories..."
mkdir -p cobol-core/compiled
mkdir -p api-gateway/logs
mkdir -p debugger/traces
mkdir -p monitoring/prometheus
mkdir -p cloud-burst/config

# Check if .env file exists
if [ ! -f .env ]; then
    echo "📝 Creating .env file from template..."
    cat > .env << 'EOF'
# SuiteCRM COBOL Bridge Configuration

# API Configuration
JWT_SECRET=your-secret-key-here-change-me
API_KEY=your-api-key-here-change-me

# Database Configuration
DATABASE_HOST=mysql
DATABASE_NAME=suitecrm
DATABASE_USER=suitecrm
DATABASE_PASSWORD=suitecrm123

# Redis Configuration
REDIS_HOST=redis
REDIS_PORT=6379

# Mainframe Configuration (Optional - for cloud burst)
MAINFRAME_HOST=your-mainframe.com
MAINFRAME_USER=your-user
MAINFRAME_PASSWORD=your-password

# Cloud Provider Configuration (Optional)
AWS_REGION=us-east-1
AZURE_REGION=eastus
GCP_REGION=us-central1

# Monitoring Configuration
ENABLE_METRICS=true
ENABLE_TRACING=true
LOG_LEVEL=info
EOF
    echo "✅ .env file created. Please update with your settings."
fi

# Pull latest images
echo "📥 Pulling Docker images..."
docker-compose pull

# Build custom images
echo "🔨 Building custom Docker images..."
docker-compose build

# Start services
echo "🚀 Starting all services..."
docker-compose up -d

# Wait for services to be ready
echo "⏳ Waiting for services to initialize..."
sleep 10

# Check service health
echo "🏥 Checking service health..."
services=("suitecrm" "api-gateway" "monitoring" "rules-editor" "debugger")
all_healthy=true

for service in "${services[@]}"; do
    if docker-compose ps | grep -q "$service.*Up"; then
        echo "✅ $service is running"
    else
        echo "❌ $service failed to start"
        all_healthy=false
    fi
done

if [ "$all_healthy" = true ]; then
    echo ""
    echo "🎉 SuiteCRM COBOL Bridge is ready!"
    echo "=================================="
    echo ""
    echo "Access the services at:"
    echo "  📊 SuiteCRM:      http://localhost:8080 (admin/admin)"
    echo "  📈 Monitoring:    http://localhost:8081"
    echo "  🎯 Rule Editor:   http://localhost:8082"
    echo "  🐛 Debugger:      http://localhost:8083"
    echo "  ☁️  Cloud Burst:   http://localhost:3003"
    echo "  📚 API Docs:      http://localhost:3000/api-docs"
    echo "  📊 Grafana:       http://localhost:3006 (admin/admin)"
    echo ""
    echo "Run 'docker-compose logs -f' to view logs"
    echo "Run 'docker-compose down' to stop all services"
    echo ""
    echo "📺 For demo walkthrough, see: demo/DEMO_SCRIPT.md"
else
    echo ""
    echo "⚠️  Some services failed to start."
    echo "Run 'docker-compose logs' to see error details."
    exit 1
fi