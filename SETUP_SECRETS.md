# Quick Setup Guide - Update Secrets

The automated setup has created placeholder secrets. Here's how to update them with real values:

## 1. Update Docker Hub Credentials (Required)

```bash
# Using GitHub CLI
gh secret set DOCKER_USERNAME --body "your-actual-docker-username"
gh secret set DOCKER_PASSWORD --body "your-actual-docker-password-or-token"
```

Or via GitHub UI:
1. Go to: https://github.com/jfuginay/suitecrm-cobol-bridge/settings/secrets/actions
2. Click on `DOCKER_USERNAME` → Update secret
3. Click on `DOCKER_PASSWORD` → Update secret

## 2. Quick Local Testing Setup

For local development, create a `.env.local` file:

```bash
cat > .env.local << 'EOF'
# Local Development Settings
DATABASE_HOST=localhost
DATABASE_NAME=suitecrm_dev
DATABASE_USER=root
DATABASE_PASSWORD=root
REDIS_HOST=localhost
REDIS_PASSWORD=
JWT_SECRET=local_dev_secret_change_me
API_KEY=local_dev_api_key_change_me
ENCRYPTION_KEY=local_dev_encryption_key
EOF
```

## 3. Environment Secrets (Via GitHub UI)

Since environment secrets need special encryption, update them via the GitHub UI:

### Development Environment
1. Go to: https://github.com/jfuginay/suitecrm-cobol-bridge/settings/environments/development
2. Click "Add secret" for each:
   - `DATABASE_HOST`: localhost
   - `DATABASE_NAME`: suitecrm_dev
   - `DATABASE_USER`: root
   - `DATABASE_PASSWORD`: your_dev_password
   - `REDIS_HOST`: localhost
   - `REDIS_PASSWORD`: (leave empty for dev)
   - `JWT_SECRET`: dev_jwt_secret_32_chars_minimum
   - `API_KEY`: dev_api_key_32_chars_minimum
   - `ENCRYPTION_KEY`: 32_character_encryption_key_here

## 4. Optional: External Services

Only if you have accounts:

```bash
# SonarQube
gh secret set SONAR_TOKEN --body "your-actual-sonar-token"

# Codecov
gh secret set CODECOV_TOKEN --body "your-actual-codecov-token"

# Slack
gh secret set SLACK_WEBHOOK_URL --body "https://hooks.slack.com/services/YOUR/ACTUAL/WEBHOOK"
```

## 5. Test Your Setup

```bash
# Test secrets access
gh workflow run test-secrets.yml --field environment=development

# Watch the run
gh run watch

# Or check in browser
open https://github.com/jfuginay/suitecrm-cobol-bridge/actions
```

## Quick Start Commands

```bash
# 1. Update Docker credentials (minimum required)
gh secret set DOCKER_USERNAME --body "your-docker-username"
gh secret set DOCKER_PASSWORD --body "your-docker-token"

# 2. Run tests
gh workflow run test-secrets.yml --field environment=development

# 3. Check results
gh run list --workflow=test-secrets.yml --limit=1
```

## What's Next?

1. **Minimum Required**: Update Docker credentials to fix the Docker login test
2. **For CI/CD**: Add environment secrets via GitHub UI
3. **For Code Quality**: Add SonarQube/Codecov tokens if you have accounts
4. **For Notifications**: Add Slack/Teams webhooks if needed

The repository secrets are already set with placeholders. You only need to update the ones you'll actually use!