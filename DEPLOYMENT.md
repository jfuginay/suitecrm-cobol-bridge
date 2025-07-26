# SuiteCRM COBOL Bridge - Production Deployment Guide

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Infrastructure Setup](#infrastructure-setup)
3. [Kubernetes Deployment](#kubernetes-deployment)
4. [Docker Compose Deployment](#docker-compose-deployment)
5. [Security Hardening](#security-hardening)
6. [Monitoring Setup](#monitoring-setup)
7. [Backup and Disaster Recovery](#backup-and-disaster-recovery)
8. [Troubleshooting](#troubleshooting)
9. [Maintenance Procedures](#maintenance-procedures)

## Prerequisites

### System Requirements

- **Kubernetes**: v1.26+ (for K8s deployment)
- **Docker**: v20.10+ and Docker Compose v2.0+ (for Docker deployment)
- **CPU**: Minimum 8 cores, recommended 16+ cores
- **Memory**: Minimum 32GB RAM, recommended 64GB+
- **Storage**: 500GB+ SSD storage with high IOPS
- **Network**: 1Gbps+ network connectivity

### Required Tools

```bash
# Install required CLI tools
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash
curl -L https://github.com/kubernetes-sigs/kustomize/releases/download/kustomize%2Fv5.0.0/kustomize_v5.0.0_linux_amd64.tar.gz | tar xz
```

### SSL Certificates

Obtain SSL certificates for:
- `suitecrm.example.com` - Main application
- `api.suitecrm.example.com` - API Gateway
- `monitoring.suitecrm.example.com` - Monitoring dashboard
- `grafana.suitecrm.example.com` - Grafana dashboard

## Infrastructure Setup

### AWS Deployment

1. **Configure AWS CLI**:
   ```bash
   aws configure
   export AWS_REGION=us-east-1
   ```

2. **Deploy Infrastructure**:
   ```bash
   cd terraform/aws
   terraform init
   terraform plan -var="environment=prod" -out=tfplan
   terraform apply tfplan
   ```

3. **Configure kubectl**:
   ```bash
   aws eks update-kubeconfig --region $AWS_REGION --name suitecrm-cobol-prod
   ```

### Azure Deployment

```bash
cd terraform/azure
terraform init
terraform plan -var="environment=prod" -out=tfplan
terraform apply tfplan
```

### GCP Deployment

```bash
cd terraform/gcp
terraform init
terraform plan -var="environment=prod" -out=tfplan
terraform apply tfplan
```

## Kubernetes Deployment

### 1. Create Namespace and Secrets

```bash
# Create namespace
kubectl apply -f kubernetes/base/namespace.yaml

# Create secrets (update values first!)
cp kubernetes/base/secrets.yaml kubernetes/base/secrets-prod.yaml
# Edit secrets-prod.yaml with production values
kubectl apply -f kubernetes/base/secrets-prod.yaml
```

### 2. Deploy Core Services

```bash
# Deploy MySQL
kubectl apply -f kubernetes/base/mysql-deployment.yaml

# Wait for MySQL to be ready
kubectl wait --for=condition=ready pod -l app=mysql -n suitecrm-cobol --timeout=300s

# Deploy Redis
kubectl apply -f kubernetes/base/redis-deployment.yaml

# Deploy ConfigMaps
kubectl apply -f kubernetes/base/configmap.yaml
```

### 3. Deploy Application Services

```bash
# Deploy API Gateway
kubectl apply -f kubernetes/base/api-gateway-deployment.yaml

# Deploy SuiteCRM
kubectl apply -f kubernetes/base/suitecrm-deployment.yaml

# Deploy Monitoring
kubectl apply -f kubernetes/base/monitoring-deployment.yaml

# Deploy Ingress
kubectl apply -f kubernetes/base/ingress.yaml

# Deploy HPA
kubectl apply -f kubernetes/base/hpa.yaml
```

### 4. Verify Deployment

```bash
# Check pod status
kubectl get pods -n suitecrm-cobol

# Check services
kubectl get svc -n suitecrm-cobol

# Check ingress
kubectl get ingress -n suitecrm-cobol

# View logs
kubectl logs -f deployment/suitecrm -n suitecrm-cobol
```

## Docker Compose Deployment

### 1. Prepare Environment

```bash
# Copy production environment file
cp .env.production .env

# Edit .env with production values
nano .env

# Create required directories
mkdir -p redis ssl nginx/ssl monitoring/grafana-dashboards
```

### 2. Prepare SSL Certificates

```bash
# Copy SSL certificates
cp /path/to/cert.pem ssl/
cp /path/to/key.pem ssl/
cp /path/to/ca-bundle.crt ssl/
```

### 3. Deploy Services

```bash
# Pull latest images
docker-compose -f docker-compose.prod.yml pull

# Start services
docker-compose -f docker-compose.prod.yml up -d

# Check service status
docker-compose -f docker-compose.prod.yml ps

# View logs
docker-compose -f docker-compose.prod.yml logs -f
```

## Security Hardening

### 1. Network Security

```bash
# Configure firewall rules
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw allow 22/tcp from <admin-ip>
sudo ufw enable
```

### 2. SELinux Configuration (RHEL/CentOS)

```bash
# Set SELinux contexts
sudo semanage fcontext -a -t httpd_sys_content_t "/var/www/html(/.*)?"
sudo restorecon -Rv /var/www/html

# Allow network connections
sudo setsebool -P httpd_can_network_connect 1
```

### 3. File Permissions

```bash
# Set proper ownership
sudo chown -R www-data:www-data /var/www/html
sudo chmod -R 755 /var/www/html
sudo chmod -R 770 /var/www/html/cache
sudo chmod -R 770 /var/www/html/upload
```

### 4. Database Security

```sql
-- Remove anonymous users
DELETE FROM mysql.user WHERE User='';

-- Disable remote root login
DELETE FROM mysql.user WHERE User='root' AND Host NOT IN ('localhost', '127.0.0.1', '::1');

-- Remove test database
DROP DATABASE IF EXISTS test;
DELETE FROM mysql.db WHERE Db='test' OR Db='test\\_%';

-- Reload privileges
FLUSH PRIVILEGES;
```

### 5. Application Security

- Enable two-factor authentication for admin users
- Configure strong password policies
- Implement IP whitelisting for admin access
- Enable audit logging
- Regular security scanning with tools like:
  ```bash
  # Run security scan
  docker run --rm -v $(pwd):/src aquasec/trivy fs /src
  ```

## Monitoring Setup

### 1. Deploy Prometheus Stack

```bash
# Add Prometheus Helm repository
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo update

# Install Prometheus Operator
helm install prometheus prometheus-community/kube-prometheus-stack \
  --namespace monitoring \
  --create-namespace \
  --values monitoring/prometheus-values.yaml
```

### 2. Configure Grafana Dashboards

1. Access Grafana at `https://grafana.suitecrm.example.com`
2. Login with admin credentials
3. Import dashboards:
   - COBOL Execution Metrics (ID: 15001)
   - SuiteCRM Performance (ID: 15002)
   - MySQL Monitoring (ID: 7362)
   - Redis Monitoring (ID: 11835)
   - Kubernetes Cluster (ID: 12114)

### 3. Set Up Alerts

```bash
# Apply alerting rules
kubectl apply -f monitoring/alerting-rules.yml

# Configure alert notifications in Grafana
# Settings -> Notification channels -> Add channel
```

### 4. Log Aggregation

```bash
# Deploy ELK Stack
helm install elastic elastic/elasticsearch \
  --namespace logging \
  --create-namespace

helm install kibana elastic/kibana \
  --namespace logging

helm install filebeat elastic/filebeat \
  --namespace logging \
  --values monitoring/filebeat-values.yaml
```

## Backup and Disaster Recovery

### 1. Automated Backups

```bash
# Create backup CronJob
kubectl apply -f - <<EOF
apiVersion: batch/v1
kind: CronJob
metadata:
  name: backup-job
  namespace: suitecrm-cobol
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: backup
            image: your-registry/cobol-backup:latest
            env:
            - name: S3_BUCKET
              value: suitecrm-cobol-backups
            - name: BACKUP_ENCRYPTION_KEY
              valueFrom:
                secretKeyRef:
                  name: backup-secret
                  key: BACKUP_ENCRYPTION_KEY
          restartPolicy: OnFailure
EOF
```

### 2. Manual Backup Procedure

```bash
# Database backup
kubectl exec -it mysql-primary-0 -n suitecrm-cobol -- \
  mysqldump -u root -p suitecrm > backup-$(date +%Y%m%d).sql

# Application files backup
kubectl exec -it suitecrm-0 -n suitecrm-cobol -- \
  tar -czf /tmp/suitecrm-files-$(date +%Y%m%d).tar.gz /var/www/html/upload

# Redis backup
kubectl exec -it redis-master-0 -n suitecrm-cobol -- \
  redis-cli BGSAVE
```

### 3. Restore Procedure

```bash
# Restore database
kubectl exec -i mysql-primary-0 -n suitecrm-cobol -- \
  mysql -u root -p suitecrm < backup-20240101.sql

# Restore files
kubectl cp backup-files.tar.gz suitecrm-0:/tmp/
kubectl exec -it suitecrm-0 -n suitecrm-cobol -- \
  tar -xzf /tmp/backup-files.tar.gz -C /

# Restore Redis
kubectl exec -it redis-master-0 -n suitecrm-cobol -- \
  redis-cli --rdb /data/dump.rdb
```

### 4. Disaster Recovery Testing

Perform monthly DR drills:

1. **Backup Verification**:
   ```bash
   # Test restore to staging environment
   ./scripts/dr-test.sh staging
   ```

2. **Failover Testing**:
   ```bash
   # Simulate primary datacenter failure
   kubectl cordon node1 node2 node3
   # Verify application continues on remaining nodes
   ```

3. **Recovery Time Objective (RTO)**: Target < 4 hours
4. **Recovery Point Objective (RPO)**: Target < 1 hour

## Troubleshooting

### Common Issues

1. **Pod Crash Loops**:
   ```bash
   kubectl describe pod <pod-name> -n suitecrm-cobol
   kubectl logs <pod-name> -n suitecrm-cobol --previous
   ```

2. **Database Connection Issues**:
   ```bash
   # Test database connectivity
   kubectl run -it --rm debug --image=mysql:8.0 --restart=Never -- \
     mysql -h mysql-primary -u suitecrm -p
   ```

3. **High Memory Usage**:
   ```bash
   # Check memory usage
   kubectl top pods -n suitecrm-cobol
   kubectl top nodes
   ```

4. **COBOL Compilation Errors**:
   ```bash
   # Check compiler logs
   kubectl logs -f deployment/cobol-compiler -n suitecrm-cobol
   ```

### Debug Mode

Enable debug mode for troubleshooting:

```bash
# Set debug environment variable
kubectl set env deployment/api-gateway DEBUG=true -n suitecrm-cobol

# View detailed logs
kubectl logs -f deployment/api-gateway -n suitecrm-cobol
```

## Maintenance Procedures

### 1. Rolling Updates

```bash
# Update application version
kubectl set image deployment/suitecrm \
  suitecrm=your-registry/suitecrm-cobol-bridge:v2.0.0 \
  -n suitecrm-cobol

# Monitor rollout
kubectl rollout status deployment/suitecrm -n suitecrm-cobol
```

### 2. Database Maintenance

```bash
# Schedule maintenance window
kubectl apply -f - <<EOF
apiVersion: v1
kind: ConfigMap
metadata:
  name: maintenance-window
  namespace: suitecrm-cobol
data:
  start: "2024-01-15T02:00:00Z"
  end: "2024-01-15T04:00:00Z"
  message: "Scheduled maintenance in progress"
EOF
```

### 3. Certificate Renewal

```bash
# Check certificate expiration
echo | openssl s_client -connect suitecrm.example.com:443 2>/dev/null | \
  openssl x509 -noout -dates

# Update certificates
kubectl create secret tls suitecrm-tls \
  --cert=new-cert.pem \
  --key=new-key.pem \
  --dry-run=client -o yaml | \
  kubectl apply -f -
```

### 4. Performance Tuning

```bash
# Analyze slow queries
kubectl exec -it mysql-primary-0 -n suitecrm-cobol -- \
  mysql -e "SELECT * FROM mysql.slow_log ORDER BY query_time DESC LIMIT 10"

# Optimize tables
kubectl exec -it mysql-primary-0 -n suitecrm-cobol -- \
  mysqlcheck -o suitecrm --all-tables
```

## Health Checks

### Automated Health Monitoring

```bash
# Deploy health check CronJob
kubectl apply -f monitoring/health-check-cronjob.yaml

# Manual health check
curl -f https://suitecrm.example.com/health || echo "Health check failed"
```

### Performance Benchmarking

```bash
# Run performance tests
docker run --rm -v $(pwd)/tests:/tests \
  loadimpact/k6 run /tests/performance-test.js
```

## Support and Escalation

### Support Tiers

1. **L1 Support**: Application issues, user access
   - Response time: < 1 hour
   - Contact: support@example.com

2. **L2 Support**: Infrastructure, performance issues
   - Response time: < 4 hours
   - Contact: infrastructure@example.com

3. **L3 Support**: Critical outages, data loss
   - Response time: < 30 minutes
   - Contact: oncall@example.com
   - Phone: +1-555-EMERGENCY

### Escalation Matrix

| Severity | Impact | Response Time | Escalation |
|----------|--------|---------------|------------|
| Critical | System down | 30 min | L3 immediately |
| High | Major feature unavailable | 2 hours | L2 -> L3 after 4h |
| Medium | Performance degradation | 4 hours | L1 -> L2 after 8h |
| Low | Minor issues | 24 hours | L1 only |

## Compliance and Auditing

### Audit Logging

All administrative actions are logged to:
- CloudWatch Logs (AWS)
- Azure Monitor (Azure)
- Cloud Logging (GCP)

### Compliance Checks

Run quarterly compliance audits:

```bash
# Generate compliance report
./scripts/compliance-audit.sh > compliance-report-$(date +%Y%m%d).pdf
```

### Data Retention

- Application logs: 90 days
- Audit logs: 7 years
- Database backups: 30 days (daily), 12 months (monthly)
- COBOL execution history: 180 days

---

For additional support or questions, please contact the DevOps team or refer to the internal wiki.