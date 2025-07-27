# Cloud Burst Scheduler - Dockerfile
FROM node:18-alpine

WORKDIR /app

# Install system dependencies and cloud CLI tools
RUN apk add --no-cache \
    python3 \
    py3-pip \
    make \
    g++ \
    curl \
    bash \
    && pip3 install awscli azure-cli

# Install gcloud CLI
RUN curl -sSL https://sdk.cloud.google.com | bash
ENV PATH $PATH:/root/google-cloud-sdk/bin

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci

# Copy source code
COPY . .

# Create necessary directories
RUN mkdir -p /app/config /app/logs /app/jobs

# Expose port
EXPOSE 3003

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3003/health', (r) => process.exit(r.statusCode === 200 ? 0 : 1))"

# Start the service
CMD ["node", "src/index.js"]