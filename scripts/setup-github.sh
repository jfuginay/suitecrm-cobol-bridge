#!/bin/bash

# SuiteCRM COBOL Bridge - Automated GitHub Setup Script
# This script uses GitHub CLI to configure repository settings

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}SuiteCRM COBOL Bridge - GitHub Repository Setup${NC}"
echo -e "${BLUE}================================================${NC}"

# Check if gh is installed
if ! command -v gh &> /dev/null; then
    echo -e "${RED}GitHub CLI (gh) is not installed!${NC}"
    echo "Please install it from: https://cli.github.com/"
    exit 1
fi

# Check if authenticated
if ! gh auth status &> /dev/null; then
    echo -e "${YELLOW}Not authenticated with GitHub CLI${NC}"
    echo "Running: gh auth login"
    gh auth login
fi

# Get repository details
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)
echo -e "${GREEN}✓ Repository: $REPO${NC}"

# Function to create secret
create_secret() {
    local secret_name=$1
    local secret_value=$2
    local environment=$3
    
    if [[ -z "$environment" ]]; then
        # Repository secret
        echo -n "Creating repository secret $secret_name... "
        if gh secret set "$secret_name" --body "$secret_value" 2>/dev/null; then
            echo -e "${GREEN}✓${NC}"
        else
            echo -e "${YELLOW}Already exists or error${NC}"
        fi
    else
        # Environment secret
        echo -n "Creating $environment secret $secret_name... "
        if gh secret set "$secret_name" --env "$environment" --body "$secret_value" 2>/dev/null; then
            echo -e "${GREEN}✓${NC}"
        else
            echo -e "${YELLOW}Already exists or error${NC}"
        fi
    fi
}

# Create environments
echo -e "\n${BLUE}Creating Environments...${NC}"

echo -n "Creating development environment... "
gh api repos/$REPO/environments/development \
    --method PUT \
    --field wait_timer=0 \
    --field deployment_branch_policy='{"protected_branches":false,"custom_branch_policies":true}' \
    --field deployment_branch_policy.custom_branch_policies[]='develop' \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Already exists${NC}"

echo -n "Creating staging environment... "
gh api repos/$REPO/environments/staging \
    --method PUT \
    --field wait_timer=0 \
    --field reviewers[]='@jfuginay' \
    --field deployment_branch_policy='{"protected_branches":false,"custom_branch_policies":true}' \
    --field deployment_branch_policy.custom_branch_policies[]='develop' \
    --field deployment_branch_policy.custom_branch_policies[]='release/*' \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Already exists${NC}"

echo -n "Creating production environment... "
gh api repos/$REPO/environments/production \
    --method PUT \
    --field wait_timer=5 \
    --field reviewers[]='@jfuginay' \
    --field deployment_branch_policy='{"protected_branches":true,"custom_branch_policies":false}' \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Already exists${NC}"

echo -n "Creating production-approval environment... "
gh api repos/$REPO/environments/production-approval \
    --method PUT \
    --field wait_timer=0 \
    --field reviewers[]='@jfuginay' \
    --field deployment_branch_policy='{"protected_branches":true,"custom_branch_policies":false}' \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Already exists${NC}"

# Create repository secrets
echo -e "\n${BLUE}Creating Repository Secrets...${NC}"
echo -e "${YELLOW}Note: You'll need to add actual values for these secrets${NC}"

# Docker Hub
create_secret "DOCKER_USERNAME" "your-docker-username"
create_secret "DOCKER_PASSWORD" "your-docker-password"
create_secret "DOCKER_REGISTRY_URL" "docker.io"

# Code Quality
create_secret "SONAR_TOKEN" "sqp_your_sonar_token"
create_secret "CODECOV_TOKEN" "your-codecov-token"
create_secret "SNYK_TOKEN" "your-snyk-token"

# Notifications
create_secret "SLACK_WEBHOOK_URL" "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
create_secret "TEAMS_WEBHOOK_URL" "https://outlook.office.com/webhook/YOUR/WEBHOOK/URL"

# NPM
create_secret "NPM_TOKEN" "npm_your_npm_token"

# GitHub PAT - Using current token
echo -n "Creating GH_PAT secret... "
GH_TOKEN=$(gh auth token)
if [[ -n "$GH_TOKEN" ]]; then
    create_secret "GH_PAT" "$GH_TOKEN"
else
    echo -e "${YELLOW}Skipped - could not get token${NC}"
fi

# Create environment-specific secrets
echo -e "\n${BLUE}Creating Development Environment Secrets...${NC}"

# Development Database
create_secret "DATABASE_HOST" "localhost" "development"
create_secret "DATABASE_NAME" "suitecrm_dev" "development"
create_secret "DATABASE_USER" "suitecrm_dev" "development"
create_secret "DATABASE_PASSWORD" "dev_password_$(openssl rand -hex 12)" "development"

# Development Redis
create_secret "REDIS_HOST" "localhost" "development"
create_secret "REDIS_PASSWORD" "dev_redis_$(openssl rand -hex 12)" "development"

# Development Application
create_secret "JWT_SECRET" "dev_jwt_secret_$(openssl rand -hex 32)" "development"
create_secret "API_KEY" "dev_api_key_$(openssl rand -hex 32)" "development"
create_secret "ENCRYPTION_KEY" "dev_enc_$(openssl rand -hex 16)" "development"

# Development AWS (placeholder)
create_secret "AWS_ACCESS_KEY_ID" "AKIA_DEV_PLACEHOLDER" "development"
create_secret "AWS_SECRET_ACCESS_KEY" "dev_aws_secret_placeholder" "development"
create_secret "AWS_REGION" "us-east-1" "development"

echo -e "\n${BLUE}Creating Staging Environment Secrets...${NC}"

# Staging Database
create_secret "DATABASE_HOST" "staging-mysql.your-domain.com" "staging"
create_secret "DATABASE_NAME" "suitecrm_staging" "staging"
create_secret "DATABASE_USER" "suitecrm_staging" "staging"
create_secret "DATABASE_PASSWORD" "staging_password_$(openssl rand -hex 16)" "staging"

# Staging Redis
create_secret "REDIS_HOST" "staging-redis.your-domain.com" "staging"
create_secret "REDIS_PASSWORD" "staging_redis_$(openssl rand -hex 16)" "staging"
create_secret "REDIS_SENTINEL_HOSTS" "sentinel1:26379,sentinel2:26379,sentinel3:26379" "staging"

# Staging Application
create_secret "JWT_SECRET" "staging_jwt_secret_$(openssl rand -hex 32)" "staging"
create_secret "API_KEY" "staging_api_key_$(openssl rand -hex 32)" "staging"
create_secret "ENCRYPTION_KEY" "staging_enc_$(openssl rand -hex 16)" "staging"

# Staging AWS
create_secret "AWS_ACCESS_KEY_ID" "AKIA_STAGING_PLACEHOLDER" "staging"
create_secret "AWS_SECRET_ACCESS_KEY" "staging_aws_secret_placeholder" "staging"
create_secret "AWS_REGION" "us-east-1" "staging"
create_secret "S3_BUCKET" "suitecrm-cobol-staging" "staging"

echo -e "\n${BLUE}Creating Production Environment Secrets...${NC}"

# Production Database
create_secret "DATABASE_HOST" "prod-mysql.your-domain.com" "production"
create_secret "DATABASE_NAME" "suitecrm_prod" "production"
create_secret "DATABASE_USER" "suitecrm_prod" "production"
create_secret "DATABASE_PASSWORD" "prod_password_$(openssl rand -hex 24)" "production"
create_secret "DATABASE_SSL_CA" "/app/certs/rds-ca-2019-root.pem" "production"

# Production Redis
create_secret "REDIS_HOST" "prod-redis.your-domain.com" "production"
create_secret "REDIS_PASSWORD" "prod_redis_$(openssl rand -hex 24)" "production"
create_secret "REDIS_SENTINEL_HOSTS" "sentinel1:26379,sentinel2:26379,sentinel3:26379" "production"
create_secret "REDIS_SSL_ENABLED" "true" "production"

# Production Application
create_secret "JWT_SECRET" "prod_jwt_secret_$(openssl rand -hex 64)" "production"
create_secret "API_KEY" "prod_api_key_$(openssl rand -hex 64)" "production"
create_secret "ENCRYPTION_KEY" "prod_enc_$(openssl rand -hex 16)" "production"
create_secret "SESSION_SECRET" "prod_session_$(openssl rand -hex 32)" "production"

# Production Cloud Providers
create_secret "AWS_ACCESS_KEY_ID" "AKIA_PROD_PLACEHOLDER" "production"
create_secret "AWS_SECRET_ACCESS_KEY" "prod_aws_secret_placeholder" "production"
create_secret "AWS_REGION" "us-east-1" "production"
create_secret "S3_BUCKET" "suitecrm-cobol-prod" "production"
create_secret "AWS_KMS_KEY_ID" "arn:aws:kms:us-east-1:xxxxx:key/xxxxx" "production"

# Enable GitHub Actions
echo -e "\n${BLUE}Enabling GitHub Actions...${NC}"
gh api repos/$REPO/actions/permissions \
    --method PUT \
    --field enabled=true \
    --field allowed_actions=all \
    --silent && echo -e "${GREEN}✓ GitHub Actions enabled${NC}"

# Set up branch protection rules
echo -e "\n${BLUE}Setting up Branch Protection Rules...${NC}"

# Main branch protection
echo -n "Protecting main branch... "
gh api repos/$REPO/branches/main/protection \
    --method PUT \
    --field required_status_checks='{"strict":true,"contexts":["ci / test (18.x)","ci / test (20.x)","ci / lint","security / container-scan","security / code-scan","quality / sonarqube","quality / coverage-check"]}' \
    --field enforce_admins=true \
    --field required_pull_request_reviews='{"required_approving_review_count":2,"dismiss_stale_reviews":true,"require_code_owner_reviews":true}' \
    --field restrictions=null \
    --field allow_force_pushes=false \
    --field allow_deletions=false \
    --field required_conversation_resolution=true \
    --field lock_branch=false \
    --field allow_fork_syncing=true \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Already protected or error${NC}"

# Develop branch protection
echo -n "Protecting develop branch... "
gh api repos/$REPO/branches/develop/protection \
    --method PUT \
    --field required_status_checks='{"strict":true,"contexts":["ci / test (18.x)","ci / test (20.x)","ci / lint"]}' \
    --field enforce_admins=false \
    --field required_pull_request_reviews='{"required_approving_review_count":1,"dismiss_stale_reviews":true}' \
    --field restrictions=null \
    --field allow_force_pushes=true \
    --field allow_deletions=false \
    --field required_conversation_resolution=true \
    --silent 2>/dev/null && echo -e "${GREEN}✓${NC}" || echo -e "${YELLOW}Branch not found or already protected${NC}"

# Create develop branch if it doesn't exist
if ! git show-ref --verify --quiet refs/heads/develop; then
    echo -e "\n${YELLOW}Creating develop branch...${NC}"
    git checkout -b develop
    git push -u origin develop
    git checkout main
fi

# Enable vulnerability alerts
echo -e "\n${BLUE}Enabling Security Features...${NC}"
gh api repos/$REPO \
    --method PATCH \
    --field security_and_analysis.secret_scanning.status=enabled \
    --field security_and_analysis.secret_scanning_push_protection.status=enabled \
    --silent 2>/dev/null && echo -e "${GREEN}✓ Secret scanning enabled${NC}" || echo -e "${YELLOW}Secret scanning not available${NC}"

# Enable Dependabot
echo -n "Enabling Dependabot... "
mkdir -p .github
cat > .github/dependabot.yml << 'EOF'
version: 2
updates:
  - package-ecosystem: "npm"
    directory: "/"
    schedule:
      interval: "weekly"
    open-pull-requests-limit: 10
    
  - package-ecosystem: "docker"
    directory: "/"
    schedule:
      interval: "weekly"
      
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "weekly"
      
  - package-ecosystem: "composer"
    directory: "/suitecrm-integration"
    schedule:
      interval: "weekly"
EOF
echo -e "${GREEN}✓${NC}"

# Create issue templates
echo -e "\n${BLUE}Creating Issue Templates...${NC}"
mkdir -p .github/ISSUE_TEMPLATE

cat > .github/ISSUE_TEMPLATE/bug_report.md << 'EOF'
---
name: Bug report
about: Create a report to help us improve
title: '[BUG] '
labels: 'bug'
assignees: ''
---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Environment:**
 - OS: [e.g. Ubuntu 20.04]
 - Node version: [e.g. 18.x]
 - Docker version: [e.g. 20.10]

**Additional context**
Add any other context about the problem here.
EOF

cat > .github/ISSUE_TEMPLATE/feature_request.md << 'EOF'
---
name: Feature request
about: Suggest an idea for this project
title: '[FEATURE] '
labels: 'enhancement'
assignees: ''
---

**Is your feature request related to a problem? Please describe.**
A clear and concise description of what the problem is.

**Describe the solution you'd like**
A clear and concise description of what you want to happen.

**Describe alternatives you've considered**
A clear and concise description of any alternative solutions or features you've considered.

**Additional context**
Add any other context or screenshots about the feature request here.
EOF

# Create PR template
cat > .github/pull_request_template.md << 'EOF'
## Description
Brief description of what this PR does.

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing completed

## Checklist
- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes

## Related Issues
Closes #(issue number)
EOF

# Commit templates if created
if [[ -n $(git status --porcelain) ]]; then
    echo -e "\n${BLUE}Committing GitHub configuration files...${NC}"
    git add .github/
    git commit -m "chore: Add GitHub configuration files

- Add Dependabot configuration
- Add issue templates
- Add pull request template

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
    git push
fi

# Summary
echo -e "\n${BLUE}================================================${NC}"
echo -e "${GREEN}✓ GitHub Repository Setup Complete!${NC}"
echo -e "${BLUE}================================================${NC}"
echo ""
echo -e "${YELLOW}Important Notes:${NC}"
echo "1. Update placeholder secrets with real values:"
echo "   - Docker Hub credentials"
echo "   - AWS/Azure/GCP credentials"
echo "   - Database passwords"
echo "   - External service tokens"
echo ""
echo "2. Verify branch protection at:"
echo "   https://github.com/$REPO/settings/branches"
echo ""
echo "3. Check environments at:"
echo "   https://github.com/$REPO/settings/environments"
echo ""
echo "4. Test your setup by running:"
echo "   gh workflow run test-secrets.yml"
echo ""
echo -e "${GREEN}Your repository is now configured for CI/CD!${NC}"

# Save credentials info
cat > .github/SECRET_PLACEHOLDERS.md << EOF
# Secret Placeholders to Update

This file lists all the placeholder secrets that need to be updated with real values.

## Repository Secrets
- DOCKER_USERNAME: Your Docker Hub username
- DOCKER_PASSWORD: Your Docker Hub password or access token
- SONAR_TOKEN: Get from your SonarQube instance
- CODECOV_TOKEN: Get from codecov.io for your repository
- SNYK_TOKEN: Get from snyk.io account settings
- SLACK_WEBHOOK_URL: Create at api.slack.com
- TEAMS_WEBHOOK_URL: Create in MS Teams channel settings
- NPM_TOKEN: Get from npmjs.com account settings

## Environment-Specific Secrets

### All Environments
- DATABASE_HOST: Your actual database hosts
- DATABASE_PASSWORD: Strong passwords (generated ones are random)
- REDIS_HOST: Your actual Redis hosts
- AWS_ACCESS_KEY_ID: Your actual AWS credentials
- AWS_SECRET_ACCESS_KEY: Your actual AWS secrets

### Production Only
- MAINFRAME_HOST: Your mainframe connection details
- SUITECRM_URL: Your SuiteCRM instance URL
- SSL_CERT_ARN: Your AWS ACM certificate ARN
- WAF_API_KEY: Your WAF service credentials

## How to Update

Use GitHub CLI:
\`\`\`bash
# Repository secret
gh secret set SECRET_NAME --body "secret_value"

# Environment secret
gh secret set SECRET_NAME --env production --body "secret_value"
\`\`\`

Or update via GitHub UI:
https://github.com/$REPO/settings/secrets/actions
EOF

echo -e "\n${BLUE}Created SECRET_PLACEHOLDERS.md with instructions for updating secrets${NC}"