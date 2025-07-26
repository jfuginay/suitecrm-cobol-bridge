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
```bash
# Repository secret
gh secret set SECRET_NAME --body "secret_value"

# Environment secret
gh secret set SECRET_NAME --env production --body "secret_value"
```

Or update via GitHub UI:
https://github.com/jfuginay/suitecrm-cobol-bridge/settings/secrets/actions
