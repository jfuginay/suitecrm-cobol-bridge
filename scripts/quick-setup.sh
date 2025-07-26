#!/bin/bash

# Quick setup script - Updates only essential secrets

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}SuiteCRM COBOL Bridge - Quick Secret Setup${NC}"
echo -e "${BLUE}================================================${NC}"

echo -e "\n${YELLOW}This will help you set up the minimum required secrets.${NC}"
echo -e "${YELLOW}You can add more secrets later as needed.${NC}\n"

# Docker Hub Setup
echo -e "${BLUE}1. Docker Hub Credentials${NC}"
echo -e "${YELLOW}   Required for pushing/pulling Docker images${NC}"
echo -n "   Do you have a Docker Hub account? (y/n): "
read -r has_docker

if [[ "$has_docker" == "y" ]]; then
    echo -n "   Enter your Docker Hub username: "
    read -r docker_username
    echo -n "   Enter your Docker Hub password or access token: "
    read -rs docker_password
    echo
    
    echo -n "   Setting Docker credentials... "
    gh secret set DOCKER_USERNAME --body "$docker_username"
    gh secret set DOCKER_PASSWORD --body "$docker_password"
    echo -e "${GREEN}✓${NC}"
else
    echo -e "   ${YELLOW}Skipping - You'll need to add these later for CI/CD to work${NC}"
fi

# Local Development Setup
echo -e "\n${BLUE}2. Local Development Setup${NC}"
echo -e "   Creating .env.local for local development..."

cat > .env.local << 'EOF'
# Local Development Configuration
# This file is for local development only - do not commit!

# Database
DATABASE_HOST=localhost
DATABASE_NAME=suitecrm_dev
DATABASE_USER=root
DATABASE_PASSWORD=root

# Redis
REDIS_HOST=localhost
REDIS_PORT=6379
REDIS_PASSWORD=

# Application
JWT_SECRET=local_dev_jwt_secret_change_me_32chars
API_KEY=local_dev_api_key_change_me_32chars
ENCRYPTION_KEY=local_encryption_key_32_chars_ok

# Features
ENABLE_DEBUG=true
LOG_LEVEL=debug
NODE_ENV=development

# Docker
DOCKER_REGISTRY_URL=docker.io
EOF

echo -e "   ${GREEN}✓ Created .env.local${NC}"

# Add to .gitignore
if ! grep -q ".env.local" .gitignore 2>/dev/null; then
    echo ".env.local" >> .gitignore
    echo -e "   ${GREEN}✓ Added .env.local to .gitignore${NC}"
fi

# Summary
echo -e "\n${BLUE}================================================${NC}"
echo -e "${GREEN}✓ Quick Setup Complete!${NC}"
echo -e "${BLUE}================================================${NC}"

echo -e "\n${YELLOW}Next Steps:${NC}"
echo "1. For CI/CD to work, you need to:"
if [[ "$has_docker" != "y" ]]; then
    echo "   - Add Docker Hub credentials:"
    echo "     gh secret set DOCKER_USERNAME --body \"your-username\""
    echo "     gh secret set DOCKER_PASSWORD --body \"your-password\""
fi
echo "   - Add environment secrets via GitHub UI:"
echo "     https://github.com/$(gh repo view --json nameWithOwner -q .nameWithOwner)/settings/environments"
echo ""
echo "2. For local development:"
echo "   - Start services: docker-compose up -d"
echo "   - Access SuiteCRM: http://localhost:8080"
echo ""
echo "3. Test your GitHub setup:"
echo "   gh workflow run test-secrets.yml --field environment=development"
echo ""
echo "4. Optional: Add external service tokens as needed"
echo "   - See SETUP_SECRETS.md for details"

# Offer to run test
echo -e "\n${YELLOW}Would you like to test your GitHub secrets now? (y/n):${NC} "
read -r run_test

if [[ "$run_test" == "y" ]]; then
    echo "Running test workflow..."
    gh workflow run test-secrets.yml --field environment=development
    echo -e "${GREEN}✓ Test workflow started!${NC}"
    echo "Check status at: https://github.com/$(gh repo view --json nameWithOwner -q .nameWithOwner)/actions"
fi