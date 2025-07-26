#!/bin/bash

# Fix environment secrets using GitHub API directly
# This script works around gh CLI limitations with environment secrets

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}Fixing Environment Secrets...${NC}"

# Get repository and token
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)
TOKEN=$(gh auth token)

# Function to create environment secret via API
create_env_secret() {
    local env_name=$1
    local secret_name=$2
    local secret_value=$3
    
    echo -n "Setting $env_name secret $secret_name... "
    
    # Get public key for the environment
    response=$(curl -s -H "Authorization: token $TOKEN" \
        "https://api.github.com/repos/$REPO/environments/$env_name/secrets/public-key")
    
    key_id=$(echo "$response" | jq -r .key_id)
    public_key=$(echo "$response" | jq -r .key)
    
    if [[ "$key_id" == "null" ]]; then
        echo -e "${YELLOW}Failed to get public key${NC}"
        return
    fi
    
    # Encrypt the secret value
    encrypted=$(echo -n "$secret_value" | openssl rsautl -encrypt -pubin -inkey <(echo "$public_key" | base64 -d) | base64)
    
    # Create or update the secret
    curl -s -X PUT \
        -H "Authorization: token $TOKEN" \
        -H "Accept: application/vnd.github.v3+json" \
        "https://api.github.com/repos/$REPO/environments/$env_name/secrets/$secret_name" \
        -d "{\"encrypted_value\":\"$encrypted\",\"key_id\":\"$key_id\"}" > /dev/null
    
    echo -e "${GREEN}✓${NC}"
}

# Check if we have required tools
if ! command -v jq &> /dev/null; then
    echo "Installing jq..."
    if [[ "$OSTYPE" == "darwin"* ]]; then
        brew install jq
    else
        sudo apt-get update && sudo apt-get install -y jq
    fi
fi

# Development environment secrets
echo -e "\n${BLUE}Setting Development Environment Secrets...${NC}"
create_env_secret "development" "DATABASE_HOST" "localhost"
create_env_secret "development" "DATABASE_NAME" "suitecrm_dev"
create_env_secret "development" "DATABASE_USER" "suitecrm_dev"
create_env_secret "development" "DATABASE_PASSWORD" "dev_password_$(openssl rand -hex 12)"
create_env_secret "development" "REDIS_HOST" "localhost"
create_env_secret "development" "REDIS_PASSWORD" "dev_redis_$(openssl rand -hex 12)"
create_env_secret "development" "JWT_SECRET" "dev_jwt_secret_$(openssl rand -hex 32)"
create_env_secret "development" "API_KEY" "dev_api_key_$(openssl rand -hex 32)"
create_env_secret "development" "ENCRYPTION_KEY" "$(openssl rand -hex 16)"

# Quick test of real Docker credentials
echo -e "\n${BLUE}Testing with real Docker Hub credentials...${NC}"
echo "Would you like to set up real Docker Hub credentials? (y/n)"
read -r response

if [[ "$response" == "y" ]]; then
    echo "Enter your Docker Hub username:"
    read -r docker_username
    echo "Enter your Docker Hub password or access token:"
    read -rs docker_password
    echo
    
    # Update Docker credentials
    gh secret set DOCKER_USERNAME --body "$docker_username"
    gh secret set DOCKER_PASSWORD --body "$docker_password"
    
    echo -e "${GREEN}✓ Docker credentials updated${NC}"
fi

echo -e "\n${GREEN}Environment secrets fixed!${NC}"
echo "Run the test workflow again to verify:"
echo "  gh workflow run test-secrets.yml --field environment=development"