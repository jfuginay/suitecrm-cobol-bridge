# COBOL Time-Travel Debugger 🕐

A revolutionary debugging system for the SuiteCRM-COBOL Bridge that makes debugging production COBOL issues as easy as debugging JavaScript. Travel through time to understand exactly what happened in your COBOL programs!

## Features

### 🎯 Core Capabilities

- **Time-Travel Debugging**: Step forward and backward through COBOL execution
- **Complete State Capture**: Records every variable value at each step
- **Timeline Visualization**: Interactive visual representation of program flow
- **Edge Case Detection**: Automatically identifies potential issues
- **Patch Generation**: Creates COBOL patches for detected problems
- **Replay Engine**: Test executions with modified inputs

### 🔍 Analysis Features

- **Pattern Detection**: Identifies infinite loops, division by zero, overflow risks
- **Variable History**: Track how variables change over time
- **Call Graph Analysis**: Visualize program flow and dependencies
- **Performance Hotspots**: Find frequently executed paragraphs
- **Comparative Analysis**: Compare multiple execution traces

### 🛠️ Developer Tools

- **WebSocket Real-time Updates**: Live debugging sessions
- **REST API**: Programmatic access to debugging features
- **Export Formats**: JSON, CSV, HTML reports
- **Search Capabilities**: Find specific patterns across traces

## Installation

```bash
# Navigate to debugger directory
cd /path/to/suitecrm-cobol-bridge/debugger

# Install dependencies
npm install

# Start the debugger server
npm start
```

The debugger UI will be available at: http://localhost:3005

## Quick Start

### 1. Recording a Trace

```javascript
// Send execution data to the debugger
POST http://localhost:3005/api/record
{
  "programName": "CALCULATE-INTEREST",
  "executionData": [
    {
      "paragraph": "MAIN-PROCEDURE",
      "variables": {
        "WS-PRINCIPAL": { "value": "10000.00", "type": "DECIMAL" },
        "WS-RATE": { "value": "5.5", "type": "DECIMAL" }
      },
      "operation": "PERFORM"
    }
    // ... more steps
  ]
}
```

### 2. Loading and Analyzing

1. Open the debugger UI
2. Click "Load Trace" to see available traces
3. Select a trace to load it into the timeline
4. Use the playback controls to navigate through execution

### 3. Detecting Edge Cases

Click "Analyze" to automatically detect:
- Infinite loops
- Division by zero risks
- Numeric overflow conditions
- Uninitialized variables
- File operation errors

### 4. Generating Patches

1. Select an issue from the edge case analysis
2. Choose a patch type:
   - Boundary Check
   - Null Check
   - Overflow Protection
   - Division Guard
3. Click "Generate Patch" to create COBOL fix code

### 5. Replay with Modified Inputs

1. Modify initial variable values in the Replay Engine
2. Click "Replay Execution" to simulate with new values
3. Compare results with original execution

## Architecture

```
debugger/
├── src/
│   ├── server.js              # Express + WebSocket server
│   ├── recorder/
│   │   └── ExecutionRecorder.js    # Records COBOL execution
│   ├── storage/
│   │   └── TimelineStorage.js      # SQLite persistence
│   ├── models/
│   │   └── Timeline.js             # Timeline data model
│   ├── generators/
│   │   └── PatchGenerator.js       # Creates COBOL patches
│   ├── analyzers/
│   │   └── EdgeCaseAnalyzer.js     # Detects edge cases
│   └── ui/
│       ├── index.html              # Main UI
│       ├── js/
│       │   ├── timeline.js         # Timeline visualization
│       │   ├── variableInspector.js # Variable state viewer
│       │   └── replayEngine.js     # Execution replay
│       └── css/
│           └── styles.css          # UI styling
├── data/                      # SQLite database storage
├── examples/
│   └── debug-trace.json       # Sample trace file
└── package.json
```

## API Reference

### Recording Traces

```http
POST /api/record
Content-Type: application/json

{
  "programName": "string",
  "executionData": [{
    "paragraph": "string",
    "section": "string",
    "line": number,
    "operation": "string",
    "variables": {},
    "memory": {},
    "fileHandles": {},
    "performedOperations": [],
    "stackTrace": [],
    "conditions": {},
    "loopIterations": number
  }]
}
```

### Listing Traces

```http
GET /api/traces?programName=CALC-INTEREST&startDate=2024-01-01
```

### Loading a Trace

```http
GET /api/traces/:traceId
```

### Searching Traces

```http
POST /api/search
{
  "pattern": "DIVIDE",
  "searchType": "paragraph|variable|pattern"
}
```

### Comparing Traces

```http
POST /api/compare
{
  "traceId1": "uuid-1",
  "traceId2": "uuid-2"
}
```

### Exporting Traces

```http
GET /api/traces/:traceId/export?format=json|csv|html
```

## WebSocket Events

### Client → Server

```javascript
// Load a trace
ws.send(JSON.stringify({
  type: 'LOAD_TRACE',
  traceId: 'uuid'
}));

// Step navigation
ws.send(JSON.stringify({ type: 'STEP_FORWARD' }));
ws.send(JSON.stringify({ type: 'STEP_BACKWARD' }));
ws.send(JSON.stringify({ type: 'GOTO_STEP', stepIndex: 42 }));

// Analysis
ws.send(JSON.stringify({ type: 'ANALYZE_EDGE_CASES' }));

// Patch generation
ws.send(JSON.stringify({
  type: 'GENERATE_PATCH',
  issueStep: 10,
  fixType: 'division-guard'
}));

// Replay
ws.send(JSON.stringify({
  type: 'REPLAY_WITH_INPUTS',
  modifiedInputs: {
    initialValues: {
      'WS-RATE': '10.5'
    }
  }
}));
```

### Server → Client

```javascript
// Timeline loaded
{
  type: 'TIMELINE_LOADED',
  timeline: { /* compact timeline data */ }
}

// Step update
{
  type: 'STEP_UPDATE',
  step: { /* step data */ },
  currentStep: 42
}

// Edge cases found
{
  type: 'EDGE_CASES',
  edgeCases: [{ /* edge case details */ }]
}

// Patch generated
{
  type: 'PATCH_GENERATED',
  patch: { /* COBOL patch code */ }
}
```

## Usage Examples

### Debugging an Infinite Loop

```javascript
// 1. Load trace with suspected infinite loop
const trace = await fetch('/api/traces/suspected-loop-trace');

// 2. Analyze for patterns
const analysis = await fetch('/api/traces/suspected-loop-trace/analyze');

// 3. Generate fix
if (analysis.edgeCases.find(e => e.type === 'INFINITE_LOOP')) {
  const patch = await generatePatch('loop-termination');
  console.log(patch);
}
```

### Testing Boundary Conditions

```javascript
// Use the replay engine to test with extreme values
const replayResult = await fetch('/api/replay', {
  method: 'POST',
  body: JSON.stringify({
    traceId: 'original-trace',
    modifiedInputs: {
      initialValues: {
        'WS-AMOUNT': '999999999',  // Max value
        'WS-RATE': '0',            // Min value
        'WS-NAME': ''              // Empty string
      }
    }
  })
});
```

### Comparing Production vs Test

```javascript
// Compare execution between environments
const comparison = await fetch('/api/compare', {
  method: 'POST',
  body: JSON.stringify({
    traceId1: 'production-trace',
    traceId2: 'test-trace'
  })
});

console.log(`Divergence at step: ${comparison.divergencePoint.stepIndex}`);
console.log(`Differences found: ${comparison.differences.length}`);
```

## Best Practices

1. **Recording Traces**
   - Capture complete variable state at each step
   - Include memory usage information
   - Record file operation status codes
   - Track loop iteration counts

2. **Performance**
   - Limit trace size to avoid memory issues
   - Use compression for large traces
   - Archive old traces regularly
   - Index frequently searched fields

3. **Security**
   - Sanitize sensitive data before recording
   - Use authentication for production debugging
   - Limit trace retention period
   - Audit debugger access

## Troubleshooting

### Common Issues

1. **Large Trace Files**
   - Use pagination when loading
   - Enable compression in storage
   - Increase Node.js memory limit

2. **WebSocket Disconnections**
   - Check firewall settings
   - Verify WebSocket port is open
   - Monitor connection status indicator

3. **Performance Issues**
   - Index paragraph names in database
   - Limit timeline visualization range
   - Use step sampling for long traces

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

## License

MIT License - See LICENSE file for details

## Support

For issues and questions:
- GitHub Issues: [Project Issues](https://github.com/example/suitecrm-cobol-bridge)
- Documentation: [Full Docs](https://docs.example.com)

---

**Making COBOL debugging a time-traveling adventure! 🚀**