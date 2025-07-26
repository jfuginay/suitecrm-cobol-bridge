#!/bin/bash

# SuiteCRM-COBOL Bridge API Gateway Startup Script

echo "Starting SuiteCRM-COBOL Bridge API Gateway..."

# Check if .env file exists
if [ ! -f .env ]; then
    echo "Creating .env file from .env.example..."
    cp .env.example .env
    echo "Please edit .env file with your configuration before starting."
    exit 1
fi

# Create required directories
mkdir -p logs temp

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "Installing dependencies..."
    npm install
fi

# Check if Redis is running
if ! redis-cli ping > /dev/null 2>&1; then
    echo "Redis is not running. Starting Redis..."
    if command -v redis-server > /dev/null; then
        redis-server --daemonize yes
        sleep 2
    else
        echo "Redis is not installed. Please install Redis or use Docker."
        exit 1
    fi
fi

# Start the application
if [ "$1" = "dev" ]; then
    echo "Starting in development mode..."
    npm run dev
elif [ "$1" = "docker" ]; then
    echo "Starting with Docker Compose..."
    docker-compose up -d
    echo "Services started. Check logs with: docker-compose logs -f"
else
    echo "Starting in production mode..."
    npm start
fi