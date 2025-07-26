# SuiteCRM-COBOL Bridge Monitoring Dashboard

A modern, real-time monitoring dashboard for the SuiteCRM-COBOL Bridge system. This React-based application provides comprehensive visibility into COBOL program execution, batch job progress, system metrics, and alerts.

## Features

### Real-time Monitoring
- **COBOL Program Status**: Live tracking of program execution with CPU, memory, and runtime metrics
- **Batch Job Progress**: Visual progress bars showing job completion status and estimated time remaining
- **Performance Metrics**: Interactive charts displaying CPU usage, memory consumption, throughput, and response times
- **Live Log Viewer**: Real-time log streaming with filtering and search capabilities
- **Alert Management**: Instant notifications for system issues with acknowledgment and dismissal options

### Technical Features
- WebSocket connections for real-time data updates
- Dark theme optimized for monitoring screens
- Responsive design for various screen sizes
- Efficient data visualization with Chart.js
- Automatic reconnection handling
- Log download functionality
- Performance-optimized rendering

## Getting Started

### Prerequisites
- Node.js 18 or higher
- npm or yarn package manager

### Installation

1. Install dependencies:
```bash
npm install
```

2. Configure environment variables:
```bash
# Create .env file
echo "REACT_APP_API_URL=http://localhost:3000/api" > .env
```

3. Start the development server:
```bash
npm start
```

The dashboard will be available at `http://localhost:3001`

### Production Build

Build the optimized production version:
```bash
npm run build
```

## Docker Deployment

### Build the Docker image:
```bash
docker build -t suitecrm-cobol-monitoring .
```

### Run the container:
```bash
docker run -d \
  --name monitoring-dashboard \
  -p 80:80 \
  --network suitecrm-network \
  suitecrm-cobol-monitoring
```

### Docker Compose Integration

Add to your `docker-compose.yml`:
```yaml
monitoring:
  build: ./monitoring
  ports:
    - "8080:80"
  depends_on:
    - api-gateway
  networks:
    - suitecrm-network
  environment:
    - REACT_APP_API_URL=http://api-gateway:3000/api
```

## Architecture

### Component Structure
```
src/
├── components/
│   ├── Dashboard.js         # Main dashboard layout
│   ├── ProgramMonitor.js    # COBOL program monitoring
│   ├── BatchJobProgress.js  # Batch job progress tracking
│   ├── MetricsChart.js      # Performance metrics visualization
│   ├── LogViewer.js         # Real-time log display
│   └── AlertPanel.js        # Alert management
├── services/
│   ├── api.js              # REST API service
│   └── websocket.js        # WebSocket connection manager
└── App.js                  # Main application component
```

### Data Flow
1. WebSocket service establishes persistent connection to API Gateway
2. Real-time events are received and distributed to components
3. Components update their state and re-render automatically
4. User interactions trigger API calls for data manipulation

## API Integration

The dashboard connects to the following endpoints:

### REST API Endpoints
- `GET /api/programs/status` - Get current program status
- `GET /api/batch/jobs` - List active batch jobs
- `GET /api/metrics` - Retrieve performance metrics
- `GET /api/alerts` - Get active alerts
- `GET /api/logs` - Fetch recent logs

### WebSocket Events
- `program:status` - Program status updates
- `batch:progress` - Batch job progress updates
- `metrics:update` - New metrics data
- `log:new` - New log entries
- `alert:new` - New alerts

## Customization

### Theme Customization
Modify colors and styling in `src/App.js`:
```javascript
const AppContainer = styled.div`
  background-color: #0f1419;  // Main background
  color: #e1e8ed;            // Primary text color
`;
```

### Chart Configuration
Customize chart appearance in `src/components/MetricsChart.js`:
```javascript
const metricTypes = [
  { id: 'cpu', label: 'CPU Usage', unit: '%', color: '#10b981' },
  // Add more metrics here
];
```

### Alert Severity Levels
Configure alert styling in `src/components/AlertPanel.js`:
```javascript
// Modify severity colors and icons
case 'critical': return '#ef4444';  // Red
case 'warning': return '#f59e0b';   // Orange
case 'info': return '#3b82f6';      // Blue
```

## Performance Optimization

- **Virtual Scrolling**: Log viewer handles thousands of entries efficiently
- **Memoization**: Components use React.memo for optimal re-rendering
- **Debouncing**: Search and filter inputs are debounced
- **Lazy Loading**: Charts load data on demand
- **WebSocket Reconnection**: Automatic reconnection with exponential backoff

## Troubleshooting

### Common Issues

1. **WebSocket Connection Failed**
   - Check API Gateway is running
   - Verify WebSocket port is not blocked
   - Check browser console for CORS errors

2. **No Data Displayed**
   - Ensure API endpoints are accessible
   - Check network tab for failed requests
   - Verify authentication token if required

3. **Performance Issues**
   - Reduce log retention limit
   - Adjust chart data points
   - Enable production build optimizations

## Development

### Available Scripts
- `npm start` - Start development server
- `npm test` - Run test suite
- `npm run build` - Create production build
- `npm run eject` - Eject from Create React App (one-way operation)

### Code Style
- Uses ESLint for code linting
- Prettier for code formatting
- Styled-components for CSS-in-JS

### Contributing
1. Fork the repository
2. Create feature branch
3. Commit changes
4. Push to branch
5. Create Pull Request

## License

This project is part of the SuiteCRM-COBOL Bridge system.