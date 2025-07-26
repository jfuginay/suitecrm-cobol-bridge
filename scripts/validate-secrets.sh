#!/bin/bash

# SuiteCRM COBOL Bridge - GitHub Secrets Validation Script
# This script validates that all required secrets are configured

echo "================================================"
echo "SuiteCRM COBOL Bridge - Secrets Validation"
echo "================================================"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
MISSING_COUNT=0
FOUND_COUNT=0

# Check if running in GitHub Actions
if [[ -n "$GITHUB_ACTIONS" ]]; then
    echo -e "${BLUE}Running in GitHub Actions environment${NC}\n"
else
    echo -e "${YELLOW}Running locally - some secrets may not be available${NC}\n"
fi

# Function to check secret
check_secret() {
    local secret_name=$1
    local secret_type=$2
    local description=$3
    
    if [[ -n "${!secret_name}" ]]; then
        echo -e "${GREEN}✓${NC} ${secret_type} - ${secret_name}"
        if [[ "$description" != "" ]]; then
            echo -e "  └─ ${description}"
        fi
        ((FOUND_COUNT++))
    else
        echo -e "${RED}✗${NC} ${secret_type} - ${secret_name}"
        if [[ "$description" != "" ]]; then
            echo -e "  └─ ${description}"
        fi
        ((MISSING_COUNT++))
    fi
}

# Repository Secrets
echo -e "${BLUE}=== Repository Secrets ===${NC}"
check_secret "DOCKER_USERNAME" "Repository" "Docker Hub username"
check_secret "DOCKER_PASSWORD" "Repository" "Docker Hub password or access token"
check_secret "DOCKER_REGISTRY_URL" "Repository" "Docker registry URL (default: docker.io)"
check_secret "GH_PAT" "Repository" "GitHub Personal Access Token for releases"
check_secret "SONAR_TOKEN" "Repository" "SonarQube authentication token"
check_secret "CODECOV_TOKEN" "Repository" "Codecov upload token"
check_secret "SNYK_TOKEN" "Repository" "Snyk authentication token"
check_secret "SLACK_WEBHOOK_URL" "Repository" "Slack webhook for notifications"
check_secret "NPM_TOKEN" "Repository" "NPM token for private packages"
echo ""

# Environment-specific secrets (if environment is specified)
if [[ -n "$ENVIRONMENT" ]]; then
    echo -e "${BLUE}=== Environment Secrets (${ENVIRONMENT}) ===${NC}"
    
    # Database
    check_secret "DATABASE_HOST" "Environment" "MySQL/MariaDB host"
    check_secret "DATABASE_NAME" "Environment" "Database name"
    check_secret "DATABASE_USER" "Environment" "Database username"
    check_secret "DATABASE_PASSWORD" "Environment" "Database password"
    
    # Redis
    check_secret "REDIS_HOST" "Environment" "Redis host"
    check_secret "REDIS_PASSWORD" "Environment" "Redis password"
    
    # Application
    check_secret "JWT_SECRET" "Environment" "JWT signing secret"
    check_secret "API_KEY" "Environment" "API authentication key"
    check_secret "ENCRYPTION_KEY" "Environment" "Data encryption key"
    
    # Cloud providers (optional)
    if [[ "$ENVIRONMENT" == "production" ]] || [[ "$ENVIRONMENT" == "staging" ]]; then
        echo -e "\n${YELLOW}Cloud Provider Secrets (Optional)${NC}"
        check_secret "AWS_ACCESS_KEY_ID" "Environment" "AWS access key"
        check_secret "AWS_SECRET_ACCESS_KEY" "Environment" "AWS secret key"
        check_secret "AZURE_CLIENT_ID" "Environment" "Azure client ID"
        check_secret "AZURE_CLIENT_SECRET" "Environment" "Azure client secret"
        check_secret "GCP_PROJECT_ID" "Environment" "GCP project ID"
    fi
    echo ""
fi

# Summary
echo -e "${BLUE}=== Summary ===${NC}"
echo -e "Secrets found: ${GREEN}${FOUND_COUNT}${NC}"
echo -e "Secrets missing: ${RED}${MISSING_COUNT}${NC}"

if [[ $MISSING_COUNT -eq 0 ]]; then
    echo -e "\n${GREEN}✓ All required secrets are configured!${NC}"
    exit 0
else
    echo -e "\n${RED}✗ Some required secrets are missing!${NC}"
    echo -e "${YELLOW}Please configure the missing secrets in your GitHub repository settings.${NC}"
    
    # Provide helpful links
    echo -e "\n${BLUE}Helpful Links:${NC}"
    echo "• Repository Secrets: https://github.com/YOUR_ORG/YOUR_REPO/settings/secrets/actions"
    echo "• Environment Secrets: https://github.com/YOUR_ORG/YOUR_REPO/settings/environments"
    echo "• Documentation: https://docs.github.com/en/actions/security-guides/encrypted-secrets"
    
    exit 1
fi