# CloudBurst - Hybrid Cloud Burst Scheduler for SuiteCRM-COBOL Bridge

CloudBurst is a sophisticated hybrid cloud burst scheduling system that seamlessly extends mainframe COBOL processing capabilities to the cloud during peak workloads. It automatically detects high mainframe utilization, spins up cloud instances with GnuCOBOL, distributes parallel-safe COBOL programs, and merges results back to the mainframe.

## Features

- **Automatic Scaling**: Detects mainframe batch queue depth and CPU/memory utilization to trigger cloud burst
- **Multi-Cloud Support**: Works with AWS, Azure, and Google Cloud Platform
- **Seamless Integration**: Maintains DB2 connectivity from cloud workers to mainframe
- **Cost Optimization**: Intelligent use of spot/preemptible instances with automatic failover
- **Real-time Monitoring**: WebSocket-based dashboard for monitoring jobs and instances
- **Result Aggregation**: Automatic merging of cloud-processed results back to mainframe datasets

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│    Mainframe    │────▶│  CloudBurst      │────▶│ Cloud Workers   │
│                 │     │  Scheduler       │     │                 │
│ - z/OS          │     │                  │     │ - AWS EC2       │
│ - COBOL Batch   │     │ - Monitoring     │     │ - Azure VMs     │
│ - DB2           │◀────│ - Job Dist.     │◀────│ - GCP Compute   │
│ - JES2          │     │ - Result Merge   │     │ - GnuCOBOL      │
└─────────────────┘     └──────────────────┘     └─────────────────┘
```

## Prerequisites

- Node.js 18+ and npm
- Redis 7.0+ for job queue management
- Docker (optional, for local testing)
- Cloud provider accounts (AWS/Azure/GCP)
- Mainframe access with:
  - SSH connectivity
  - DB2 client access
  - JES2 job submission privileges

## Installation

1. Clone the repository:
```bash
git clone https://github.com/company/suitecrm-cobol-bridge.git
cd suitecrm-cobol-bridge/cloud-burst
```

2. Install dependencies:
```bash
npm install
```

3. Configure environment variables:
```bash
cp .env.example .env
# Edit .env with your configuration
```

4. Deploy infrastructure (using Terraform):
```bash
cd terraform
terraform init
terraform plan
terraform apply
```

5. Build and push worker Docker image:
```bash
docker build -f docker/Dockerfile.worker -t cobol-worker:latest .
docker tag cobol-worker:latest your-registry/cobol-worker:latest
docker push your-registry/cobol-worker:latest
```

## Configuration

### Environment Variables

```bash
# Mainframe Configuration
MAINFRAME_HOST=mainframe.company.com
MAINFRAME_USER=your-user
MAINFRAME_PASSWORD=your-password

# DB2 Configuration
DB2_HOST=mainframe.company.com
DB2_PORT=50000
DB2_DATABASE=PRODDB
DB2_USER=db2-user
DB2_PASSWORD=db2-password

# Cloud Provider Configuration
CLOUD_PROVIDER=aws  # aws, azure, gcp, or multi
AWS_REGION=us-east-1
AWS_ACCESS_KEY_ID=your-key
AWS_SECRET_ACCESS_KEY=your-secret

# Redis Configuration
REDIS_URL=redis://localhost:6379

# Scheduler Configuration
MAX_CLOUD_INSTANCES=50
USE_SPOT_INSTANCES=true
SCALE_THRESHOLD=0.8
SCALE_DOWN_THRESHOLD=0.3
```

### Cloud Provider Setup

#### AWS
1. Create an AMI with GnuCOBOL and dependencies installed
2. Set up VPC with connectivity to mainframe
3. Configure security groups for worker instances
4. Create IAM roles with necessary permissions

#### Azure
1. Create a custom VM image with GnuCOBOL
2. Set up Virtual Network with ExpressRoute to mainframe
3. Configure Network Security Groups
4. Set up Managed Identity for workers

#### GCP
1. Create a custom image with GnuCOBOL
2. Set up VPC with Cloud Interconnect to mainframe
3. Configure firewall rules
4. Set up Service Account for workers

## Usage

### Starting the Scheduler

```bash
npm start
```

Or with PM2 for production:
```bash
pm2 start ecosystem.config.js
```

### API Endpoints

- `GET /health` - Health check
- `GET /api/status` - Get scheduler status
- `POST /api/scale/up` - Manually scale up instances
- `POST /api/scale/down` - Manually scale down instances
- `GET /api/jobs` - Get job metrics
- `GET /api/instances` - List all instances
- `GET /api/costs` - Get cost analysis report

### WebSocket Events

Connect to the WebSocket server for real-time updates:

```javascript
const socket = io('http://cloudburst-server:3000');

socket.on('scheduler:status', (status) => {
  console.log('Scheduler status:', status);
});

socket.on('scheduler:scaled_up', (data) => {
  console.log('Scaled up:', data);
});
```

## COBOL Program Requirements

For a COBOL program to be eligible for cloud burst execution, it must:

1. Be marked as `CLOUD_ELIGIBLE='Y'` in the program catalog
2. Be `PARALLEL_SAFE='Y'` (no dependencies on other running jobs)
3. Have defined data requirements in the catalog
4. Use standard COBOL I/O for file access

Example eligible program:
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-INTEREST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT RESULT-FILE ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCOUNT-NUMBER    PIC 9(10).
           05  BALANCE          PIC 9(15)V99.
           05  INTEREST-RATE    PIC 9(3)V99.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT RESULT-FILE
           
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM CALCULATE-AND-WRITE
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           CLOSE RESULT-FILE
           STOP RUN.
```

## Monitoring

### Prometheus Metrics

CloudBurst exposes Prometheus metrics at `/metrics`:

- `cloudburst_jobs_scheduled_total` - Total jobs scheduled
- `cloudburst_jobs_completed_total` - Total jobs completed
- `cloudburst_jobs_failed_total` - Total jobs failed
- `cloudburst_instances_active` - Current active instances
- `cloudburst_queue_depth` - Current job queue depth
- `cloudburst_cost_hourly` - Current hourly cost

### Logging

Logs are written to:
- Console (structured JSON format)
- `logs/cloud-burst.log` - All logs
- `logs/cloud-burst-error.log` - Errors only

### Alerts

Configure alerts in your `.env` file:
```bash
ALERT_EMAIL=ops@company.com
SLACK_WEBHOOK=https://hooks.slack.com/services/xxx
PAGERDUTY_KEY=your-key
```

## Cost Optimization

CloudBurst includes automatic cost optimization features:

1. **Spot Instance Usage**: Automatically uses spot instances when available
2. **Right-Sizing**: Monitors instance utilization and recommends size changes
3. **Scheduled Scaling**: Reduces instances during off-hours
4. **Multi-Cloud Arbitrage**: Routes workloads to the cheapest provider

View cost reports:
```bash
curl http://localhost:3000/api/costs
```

## Troubleshooting

### Common Issues

1. **Connection to mainframe failed**
   - Check firewall rules and network connectivity
   - Verify SSH credentials
   - Test DB2 connection string

2. **Jobs failing on cloud workers**
   - Check COBOL program compatibility
   - Verify data file access permissions
   - Review worker logs in CloudWatch/Azure Monitor

3. **High costs**
   - Review cost optimization recommendations
   - Check for zombie instances
   - Verify spot instance usage

### Debug Mode

Enable debug logging:
```bash
LOG_LEVEL=debug npm start
```

### Health Checks

Check component health:
```bash
# Scheduler health
curl http://localhost:3000/health

# Worker health (on worker instance)
curl http://worker-ip:8080/health
```

## Development

### Running Tests

```bash
# Unit tests
npm test

# Integration tests
npm run test:integration

# Load tests
npm run test:load
```

### Local Development

Use Docker Compose for local development:
```bash
docker-compose up -d
```

This starts:
- CloudBurst scheduler
- Redis for job queue
- Mock mainframe API
- Local Docker workers

## Security

- All mainframe connections use SSH with key-based authentication
- DB2 connections support SSL/TLS
- Cloud instances use IAM roles/Managed Identities
- Data in transit is encrypted
- Temporary datasets are encrypted at rest

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Support

- Documentation: https://docs.company.com/cloudburst
- Issues: https://github.com/company/suitecrm-cobol-bridge/issues
- Slack: #cloudburst-support

## Roadmap

- [ ] Kubernetes operator for container-based workers
- [ ] Machine learning for predictive scaling
- [ ] Support for CICS transactions
- [ ] Real-time COBOL debugging in cloud
- [ ] Integration with mainframe automation tools