# SuiteCRM-COBOL Bridge API Gateway

A production-ready REST API gateway that provides a modern interface for executing COBOL programs from SuiteCRM.

## Features

- RESTful API endpoints for COBOL program execution
- JWT-based authentication and API key support
- Rate limiting and request throttling
- Redis-based caching and session management
- WebSocket support for real-time updates
- Comprehensive logging and monitoring
- Prometheus metrics integration
- OpenAPI/Swagger documentation
- Docker containerization
- Health checks and graceful shutdown

## Prerequisites

- Node.js 16+ (for local development)
- Docker and Docker Compose (for containerized deployment)
- Redis 6+
- GNU COBOL runtime

## Quick Start

### Local Development

1. Clone the repository and navigate to the API gateway directory:
```bash
cd api-gateway
```

2. Install dependencies:
```bash
npm install
```

3. Copy the environment file and configure:
```bash
cp .env.example .env
# Edit .env with your configuration
```

4. Start Redis (if not using Docker):
```bash
redis-server
```

5. Start the API gateway:
```bash
npm run dev
```

The API will be available at `http://localhost:3000`

### Docker Deployment

1. Build and start all services:
```bash
docker-compose up -d
```

This will start:
- API Gateway on port 3000
- Redis on port 6379
- Nginx reverse proxy on ports 80/443

## API Documentation

Once the server is running, access the interactive API documentation at:
```
http://localhost:3000/api-docs
```

## Authentication

### JWT Authentication

1. Login to get access token:
```bash
curl -X POST http://localhost:3000/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "user", "password": "user123"}'
```

2. Use the token in subsequent requests:
```bash
curl -X POST http://localhost:3000/api/v1/cobol/credit/calculate \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "customerId": "CUST001",
    "creditAmount": 50000,
    "income": 75000,
    "existingDebt": 10000
  }'
```

### API Key Authentication

Include the API key in the header:
```bash
curl -X POST http://localhost:3000/api/v1/cobol/credit/calculate \
  -H "X-API-Key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"customerId": "CUST001", ...}'
```

## Available Endpoints

### Authentication
- `POST /api/v1/auth/login` - User login
- `POST /api/v1/auth/refresh` - Refresh access token
- `POST /api/v1/auth/logout` - User logout
- `GET /api/v1/auth/me` - Get current user

### COBOL Programs
- `POST /api/v1/cobol/credit/calculate` - Credit calculation
- `POST /api/v1/cobol/payroll/calculate` - Payroll calculation
- `POST /api/v1/cobol/interest/calculate` - Interest calculation
- `POST /api/v1/cobol/batch/process` - Submit batch job
- `GET /api/v1/cobol/batch/status/:jobId` - Get batch job status

### Monitoring
- `GET /api/v1/monitoring/health` - Health check
- `GET /api/v1/monitoring/health/detailed` - Detailed health (authenticated)
- `GET /api/v1/monitoring/metrics` - Prometheus metrics
- `GET /api/v1/monitoring/stats` - System statistics

## WebSocket Support

Connect to WebSocket for real-time updates:

```javascript
const ws = new WebSocket('ws://localhost:3000');

ws.on('open', () => {
  // Authenticate
  ws.send(JSON.stringify({
    type: 'auth',
    token: 'YOUR_JWT_TOKEN'
  }));
});

ws.on('message', (data) => {
  const message = JSON.parse(data);
  console.log('Received:', message);
});
```

## Configuration

Key environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `PORT` | Server port | 3000 |
| `JWT_SECRET` | JWT signing secret | - |
| `REDIS_URL` | Redis connection URL | redis://localhost:6379 |
| `COBOL_BIN_PATH` | Path to COBOL binaries | ../cobol/bin |
| `LOG_LEVEL` | Logging level | info |
| `RATE_LIMIT_GENERAL` | General rate limit | 100 |

See `.env.example` for all configuration options.

## Monitoring

### Health Checks

Basic health check:
```bash
curl http://localhost:3000/health
```

Detailed health check (requires authentication):
```bash
curl http://localhost:3000/api/v1/monitoring/health/detailed \
  -H "Authorization: Bearer YOUR_TOKEN"
```

### Metrics

Prometheus metrics are available at:
```
http://localhost:3000/api/v1/monitoring/metrics
```

## Development

### Running Tests
```bash
npm test
```

### Linting
```bash
npm run lint
```

### Building for Production
```bash
docker build -t suitecrm-cobol-api .
```

## Troubleshooting

### Common Issues

1. **COBOL program not found**
   - Ensure COBOL binaries are in the configured path
   - Check `COBOL_BIN_PATH` environment variable

2. **Redis connection failed**
   - Verify Redis is running
   - Check `REDIS_URL` configuration

3. **Rate limit exceeded**
   - Wait for the rate limit window to reset
   - Consider upgrading to a higher tier

### Logs

Check application logs:
```bash
# Local development
tail -f logs/combined.log

# Docker
docker logs suitecrm-cobol-api
```

## Security Considerations

- Always use HTTPS in production
- Rotate JWT secrets regularly
- Keep dependencies updated
- Use strong passwords
- Enable rate limiting
- Monitor for suspicious activity

## License

MIT License - see LICENSE file for details