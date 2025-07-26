# GitHub Environment and Secrets Setup Guide

This guide walks you through setting up GitHub environments and secrets for the SuiteCRM COBOL Bridge CI/CD pipeline.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [GitHub Environments Setup](#github-environments-setup)
3. [Repository Secrets](#repository-secrets)
4. [Environment-Specific Secrets](#environment-specific-secrets)
5. [Branch Protection Rules](#branch-protection-rules)
6. [Required Status Checks](#required-status-checks)
7. [Testing Your Setup](#testing-your-setup)
8. [Troubleshooting](#troubleshooting)

## Prerequisites

- GitHub repository admin access
- AWS/Azure/GCP accounts (for cloud deployments)
- Docker Hub account
- Slack webhook URL (optional, for notifications)
- SonarQube token (optional, for code quality)

## GitHub Environments Setup

### Step 1: Access Environment Settings

1. Navigate to your repository on GitHub
2. Click on **Settings** → **Environments**
3. Click **New environment**

### Step 2: Create Environments

Create the following environments:

#### 1. **development**
- **Required Reviewers**: None
- **Deployment branches**: `develop` branch only
- **Environment URL**: `https://dev.your-domain.com`

#### 2. **staging**
- **Required Reviewers**: 1 (Team Lead)
- **Deployment branches**: `develop` and `release/*` branches
- **Environment URL**: `https://staging.your-domain.com`
- **Wait timer**: 0 minutes

#### 3. **production**
- **Required Reviewers**: 2 (Team Lead + DevOps Engineer)
- **Deployment branches**: `main` branch only
- **Environment URL**: `https://app.your-domain.com`
- **Wait timer**: 5 minutes (allows for last-minute cancellation)

#### 4. **production-approval**
- **Required Reviewers**: 2 (Manager + DevOps Lead)
- **Deployment branches**: `main` branch only
- **Protection rules**: Enable "Required reviewers"

### Step 3: Configure Environment Protection Rules

For each environment:
1. Click on the environment name
2. Under **Environment protection rules**:
   - ✅ Required reviewers (set appropriate number)
   - ✅ Restrict deployments to selected users/teams (optional)
   - ✅ Allow administrators to bypass protection rules (recommended)

## Repository Secrets

### Step 1: Access Secrets Settings

1. Go to **Settings** → **Secrets and variables** → **Actions**
2. Click **New repository secret**

### Step 2: Add Repository-Level Secrets

Add these secrets that apply to all environments:

```bash
# Docker Registry
DOCKER_REGISTRY_URL=docker.io
DOCKER_USERNAME=your-docker-username
DOCKER_PASSWORD=your-docker-password

# GitHub
GITHUB_TOKEN=${{ secrets.GITHUB_TOKEN }}  # Auto-provided by GitHub
GH_PAT=ghp_xxxxxxxxxxxx  # Personal Access Token for releases

# Code Quality
SONAR_TOKEN=sqp_xxxxxxxxxxxxx
CODECOV_TOKEN=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

# Container Scanning
SNYK_TOKEN=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

# Notifications
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/xxx/xxx/xxx
TEAMS_WEBHOOK_URL=https://outlook.office.com/webhook/xxx

# NPM (if using private packages)
NPM_TOKEN=npm_xxxxxxxxxxxxx
```

### Step 3: Repository Secret Values

Here's how to obtain each secret:

#### Docker Hub
1. Log in to [Docker Hub](https://hub.docker.com)
2. Go to Account Settings → Security → Access Tokens
3. Create a new access token
4. Use your username and token as password

#### GitHub Personal Access Token
1. Go to GitHub Settings → Developer settings → Personal access tokens
2. Generate new token (classic)
3. Select scopes: `repo`, `write:packages`, `delete:packages`

#### SonarQube Token
1. Log in to your SonarQube instance
2. Go to My Account → Security → Tokens
3. Generate a new token

#### Codecov Token
1. Log in to [Codecov](https://codecov.io)
2. Navigate to your repository
3. Settings → General → Repository Upload Token

#### Snyk Token
1. Log in to [Snyk](https://snyk.io)
2. Account Settings → General → Auth Token

#### Slack Webhook
1. Go to [Slack API](https://api.slack.com/apps)
2. Create new app → Incoming Webhooks
3. Add new webhook to workspace

## Environment-Specific Secrets

### Development Environment Secrets

Navigate to **Settings** → **Environments** → **development** → **Add secret**

```bash
# Database
DATABASE_HOST=dev-mysql.your-domain.com
DATABASE_NAME=suitecrm_dev
DATABASE_USER=suitecrm_dev
DATABASE_PASSWORD=dev_password_here

# Redis
REDIS_HOST=dev-redis.your-domain.com
REDIS_PASSWORD=dev_redis_password

# AWS (Development)
AWS_ACCESS_KEY_ID=AKIA_DEV_xxxxx
AWS_SECRET_ACCESS_KEY=dev_secret_xxxxx
AWS_REGION=us-east-1

# Application
JWT_SECRET=dev_jwt_secret_change_me
API_KEY=dev_api_key_change_me
ENCRYPTION_KEY=dev_encryption_key_32_chars_long

# Feature Flags
ENABLE_DEBUG=true
ENABLE_MONITORING=true
LOG_LEVEL=debug
```

### Staging Environment Secrets

Navigate to **Settings** → **Environments** → **staging** → **Add secret**

```bash
# Database
DATABASE_HOST=staging-mysql.your-domain.com
DATABASE_NAME=suitecrm_staging
DATABASE_USER=suitecrm_staging
DATABASE_PASSWORD=staging_password_here

# Redis Cluster
REDIS_HOST=staging-redis-cluster.your-domain.com
REDIS_PASSWORD=staging_redis_password
REDIS_SENTINEL_HOSTS=sentinel1:26379,sentinel2:26379,sentinel3:26379

# AWS (Staging)
AWS_ACCESS_KEY_ID=AKIA_STAGING_xxxxx
AWS_SECRET_ACCESS_KEY=staging_secret_xxxxx
AWS_REGION=us-east-1
S3_BUCKET=suitecrm-cobol-staging

# Application
JWT_SECRET=staging_jwt_secret_64_chars
API_KEY=staging_api_key_64_chars
ENCRYPTION_KEY=staging_encryption_key_32_chars

# External Services
MAINFRAME_HOST=staging-mainframe.your-domain.com
MAINFRAME_USER=staging_user
MAINFRAME_PASSWORD=staging_mainframe_pass

# Monitoring
GRAFANA_API_KEY=staging_grafana_key
PROMETHEUS_REMOTE_WRITE_URL=https://prometheus-staging.your-domain.com
```

### Production Environment Secrets

Navigate to **Settings** → **Environments** → **production** → **Add secret**

```bash
# Database (Production RDS)
DATABASE_HOST=prod-mysql-cluster.xxxxx.rds.amazonaws.com
DATABASE_NAME=suitecrm_prod
DATABASE_USER=suitecrm_prod
DATABASE_PASSWORD=STRONG_PROD_PASSWORD_HERE
DATABASE_SSL_CA=/app/certs/rds-ca-2019-root.pem

# Redis Cluster (Production)
REDIS_HOST=prod-redis-cluster.xxxxx.cache.amazonaws.com
REDIS_PASSWORD=STRONG_REDIS_PASSWORD
REDIS_SENTINEL_HOSTS=sentinel1:26379,sentinel2:26379,sentinel3:26379
REDIS_SSL_ENABLED=true

# AWS (Production)
AWS_ACCESS_KEY_ID=AKIA_PROD_xxxxx
AWS_SECRET_ACCESS_KEY=PROD_SECRET_xxxxx
AWS_REGION=us-east-1
S3_BUCKET=suitecrm-cobol-prod
AWS_KMS_KEY_ID=arn:aws:kms:us-east-1:xxxxx:key/xxxxx

# Azure (Multi-cloud)
AZURE_CLIENT_ID=xxxxx-xxxx-xxxx-xxxx
AZURE_CLIENT_SECRET=xxxxx
AZURE_TENANT_ID=xxxxx-xxxx-xxxx-xxxx
AZURE_SUBSCRIPTION_ID=xxxxx-xxxx-xxxx-xxxx

# GCP (Multi-cloud)
GCP_PROJECT_ID=suitecrm-cobol-prod
GCP_SERVICE_ACCOUNT_KEY={"type":"service_account",...}

# Application (Production)
JWT_SECRET=PROD_JWT_SECRET_128_CHARS_LONG_CHANGE_ME
API_KEY=PROD_API_KEY_128_CHARS_LONG_CHANGE_ME
ENCRYPTION_KEY=PROD_ENCRYPTION_KEY_32_CHARS
SESSION_SECRET=PROD_SESSION_SECRET_64_CHARS

# Mainframe Connection
MAINFRAME_HOST=mainframe.your-company.com
MAINFRAME_USER=PRODUSER
MAINFRAME_PASSWORD=PROD_MAINFRAME_PASSWORD
MAINFRAME_PORT=23

# External Services
SUITECRM_URL=https://crm.your-company.com
SUITECRM_API_KEY=prod_suitecrm_api_key
LDAP_URL=ldaps://ldap.your-company.com:636
LDAP_BIND_DN=cn=service,dc=company,dc=com
LDAP_BIND_PASSWORD=ldap_service_password

# Monitoring & Alerting
GRAFANA_API_KEY=prod_grafana_api_key
PROMETHEUS_REMOTE_WRITE_URL=https://prometheus.your-company.com
PAGERDUTY_INTEGRATION_KEY=xxxxx
DATADOG_API_KEY=xxxxx
NEW_RELIC_LICENSE_KEY=xxxxx

# Security
WAF_API_KEY=prod_waf_key
VULNERABILITY_DB_KEY=prod_vuln_key
SSL_CERT_ARN=arn:aws:acm:us-east-1:xxxxx:certificate/xxxxx

# Feature Flags
ENABLE_DEBUG=false
ENABLE_MONITORING=true
ENABLE_CLOUD_BURST=true
ENABLE_AI_FEATURES=true
LOG_LEVEL=warn
RATE_LIMIT_REQUESTS=1000
RATE_LIMIT_WINDOW=60000
```

## Branch Protection Rules

### Step 1: Configure Main Branch Protection

1. Go to **Settings** → **Branches**
2. Click **Add rule**
3. Branch name pattern: `main`

Configure these settings:

```yaml
✅ Require a pull request before merging
  ✅ Require approvals: 2
  ✅ Dismiss stale pull request approvals when new commits are pushed
  ✅ Require review from CODEOWNERS
  ✅ Restrict who can dismiss pull request reviews

✅ Require status checks to pass before merging
  ✅ Require branches to be up to date before merging
  Required status checks:
  - ci / test (Node.js 18.x)
  - ci / test (Node.js 20.x)
  - ci / lint
  - security / container-scan
  - security / code-scan
  - quality / sonarqube
  - quality / coverage-check

✅ Require conversation resolution before merging
✅ Require signed commits
✅ Require linear history
✅ Include administrators
✅ Restrict who can push to matching branches
  - Teams: admin-team, release-team
```

### Step 2: Configure Develop Branch Protection

1. Create another rule for `develop`
2. Similar settings but with:
   - Require approvals: 1
   - Allow force pushes (for admins only)

### Step 3: Configure Release Branch Protection

1. Pattern: `release/*`
2. Settings:
   - Require approvals: 2
   - No force pushes allowed
   - Require all checks to pass

## Required Status Checks

### Configure Required Checks

In your repository settings, ensure these GitHub Actions workflows are required:

1. **CI Checks**:
   - `ci / test (18.x)`
   - `ci / test (20.x)`
   - `ci / lint`
   - `ci / build`

2. **Security Checks**:
   - `security / container-scan`
   - `security / code-scan`
   - `security / dependency-check`

3. **Quality Checks**:
   - `quality / sonarqube`
   - `quality / coverage-check`
   - `quality / performance-test`

## Testing Your Setup

### Step 1: Test Secret Access

Create a test workflow `.github/workflows/test-secrets.yml`:

```yaml
name: Test Secrets Access

on:
  workflow_dispatch:

jobs:
  test-secrets:
    runs-on: ubuntu-latest
    environment: development
    steps:
      - name: Test Repository Secrets
        run: |
          echo "Testing repository secrets..."
          echo "Docker username length: ${#DOCKER_USERNAME}"
          echo "Slack webhook exists: $([[ -n "$SLACK_WEBHOOK_URL" ]] && echo "Yes" || echo "No")"
        env:
          DOCKER_USERNAME: ${{ secrets.DOCKER_USERNAME }}
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
      
      - name: Test Environment Secrets
        run: |
          echo "Testing environment secrets..."
          echo "Database host: ${DATABASE_HOST:0:10}..."
          echo "JWT secret length: ${#JWT_SECRET}"
        env:
          DATABASE_HOST: ${{ secrets.DATABASE_HOST }}
          JWT_SECRET: ${{ secrets.JWT_SECRET }}
```

### Step 2: Verify Branch Protection

1. Create a test PR to `main` without reviews
2. Verify it cannot be merged
3. Verify all required checks must pass

### Step 3: Test Environment Deployments

1. Push to `develop` branch
2. Verify development deployment triggers automatically
3. Create PR to `main`
4. Verify production deployment requires approval

## Troubleshooting

### Common Issues

1. **Secret not found**
   - Check secret name spelling (case-sensitive)
   - Verify environment is specified in workflow
   - Check if secret is in correct scope (repository vs environment)

2. **Deployment blocked**
   - Verify branch protection rules
   - Check required reviewers are available
   - Ensure all status checks are passing

3. **Environment not accessible**
   - Verify environment protection rules
   - Check branch name patterns
   - Ensure user has deployment permissions

### Secret Validation Script

Create `scripts/validate-secrets.sh`:

```bash
#!/bin/bash

echo "Validating GitHub Secrets Configuration..."

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Check required repository secrets
REQUIRED_REPO_SECRETS=(
  "DOCKER_USERNAME"
  "DOCKER_PASSWORD"
  "SLACK_WEBHOOK_URL"
)

# Check required environment secrets
REQUIRED_ENV_SECRETS=(
  "DATABASE_HOST"
  "DATABASE_PASSWORD"
  "REDIS_HOST"
  "JWT_SECRET"
  "API_KEY"
)

echo -e "${YELLOW}Checking repository secrets...${NC}"
for secret in "${REQUIRED_REPO_SECRETS[@]}"; do
  if [[ -n "${!secret}" ]]; then
    echo -e "${GREEN}✓${NC} $secret is set"
  else
    echo -e "${RED}✗${NC} $secret is missing"
  fi
done

echo -e "\n${YELLOW}Checking environment secrets...${NC}"
for secret in "${REQUIRED_ENV_SECRETS[@]}"; do
  if [[ -n "${!secret}" ]]; then
    echo -e "${GREEN}✓${NC} $secret is set"
  else
    echo -e "${RED}✗${NC} $secret is missing"
  fi
done
```

## Security Best Practices

1. **Rotate Secrets Regularly**
   - Set calendar reminders for quarterly rotation
   - Use strong, unique passwords
   - Document rotation in changelog

2. **Least Privilege Access**
   - Only grant necessary permissions
   - Use separate credentials per environment
   - Audit access regularly

3. **Secret Management**
   - Never commit secrets to code
   - Use environment-specific secrets
   - Enable secret scanning on repository

4. **Monitoring**
   - Set up alerts for failed deployments
   - Monitor secret usage
   - Track deployment history

## Next Steps

1. Complete all secret configuration
2. Test each environment deployment
3. Document any custom secrets for your team
4. Set up secret rotation schedule
5. Configure monitoring alerts

## Additional Resources

- [GitHub Environments Documentation](https://docs.github.com/en/actions/deployment/targeting-different-environments/using-environments-for-deployment)
- [GitHub Encrypted Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets)
- [Branch Protection Rules](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/defining-the-mergeability-of-pull-requests)
- [CODEOWNERS File](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)