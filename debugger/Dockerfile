# Time-Travel Debugger - Dockerfile
FROM node:18-alpine

WORKDIR /app

# Install system dependencies
RUN apk add --no-cache python3 make g++

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci

# Copy source code
COPY . .

# Create necessary directories
RUN mkdir -p /app/traces /app/logs

# Build static assets if needed
RUN if [ -f "build.js" ]; then node build.js; fi

# Expose ports
EXPOSE 3005 80

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3005/health', (r) => process.exit(r.statusCode === 200 ? 0 : 1))"

# Start the service
CMD ["node", "src/server.js"]