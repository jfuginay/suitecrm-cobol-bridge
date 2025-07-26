const express = require('express');
const WebSocket = require('ws');
const cors = require('cors');
const bodyParser = require('body-parser');
const compression = require('compression');
const path = require('path');
const { v4: uuidv4 } = require('uuid');

const ExecutionRecorder = require('./recorder/ExecutionRecorder');
const TimelineStorage = require('./storage/TimelineStorage');
const PatchGenerator = require('./generators/PatchGenerator');
const EdgeCaseAnalyzer = require('./analyzers/EdgeCaseAnalyzer');

const app = express();
const PORT = process.env.PORT || 3005;

// Middleware
app.use(cors());
app.use(compression());
app.use(bodyParser.json({ limit: '50mb' }));
app.use(express.static(path.join(__dirname, 'ui')));

// Initialize components
const timelineStorage = new TimelineStorage();
const executionRecorder = new ExecutionRecorder(timelineStorage);
const patchGenerator = new PatchGenerator();
const edgeCaseAnalyzer = new EdgeCaseAnalyzer();

// WebSocket server for real-time debugging
const server = app.listen(PORT, () => {
    console.log(`Time-Travel Debugger running on port ${PORT}`);
    console.log(`UI available at http://localhost:${PORT}`);
});

const wss = new WebSocket.Server({ server });

// Active debugging sessions
const sessions = new Map();

wss.on('connection', (ws) => {
    const sessionId = uuidv4();
    console.log(`New debugging session: ${sessionId}`);

    sessions.set(sessionId, {
        ws,
        currentStep: 0,
        timeline: null,
        breakpoints: new Set()
    });

    ws.on('message', async (message) => {
        try {
            const data = JSON.parse(message);
            const session = sessions.get(sessionId);

            switch (data.type) {
                case 'LOAD_TRACE':
                    const timeline = await timelineStorage.loadTrace(data.traceId);
                    session.timeline = timeline;
                    ws.send(JSON.stringify({
                        type: 'TIMELINE_LOADED',
                        timeline: timeline.getCompactView()
                    }));
                    break;

                case 'STEP_FORWARD':
                    if (session.timeline) {
                        const step = session.timeline.stepForward(session.currentStep);
                        if (step) {
                            session.currentStep++;
                            ws.send(JSON.stringify({
                                type: 'STEP_UPDATE',
                                step: step,
                                currentStep: session.currentStep
                            }));
                        }
                    }
                    break;

                case 'STEP_BACKWARD':
                    if (session.timeline && session.currentStep > 0) {
                        session.currentStep--;
                        const step = session.timeline.getStep(session.currentStep);
                        ws.send(JSON.stringify({
                            type: 'STEP_UPDATE',
                            step: step,
                            currentStep: session.currentStep
                        }));
                    }
                    break;

                case 'GOTO_STEP':
                    if (session.timeline) {
                        session.currentStep = data.stepIndex;
                        const step = session.timeline.getStep(session.currentStep);
                        ws.send(JSON.stringify({
                            type: 'STEP_UPDATE',
                            step: step,
                            currentStep: session.currentStep
                        }));
                    }
                    break;

                case 'SET_BREAKPOINT':
                    session.breakpoints.add(data.paragraph);
                    break;

                case 'ANALYZE_EDGE_CASES':
                    if (session.timeline) {
                        const edgeCases = edgeCaseAnalyzer.analyze(session.timeline);
                        ws.send(JSON.stringify({
                            type: 'EDGE_CASES',
                            edgeCases
                        }));
                    }
                    break;

                case 'GENERATE_PATCH':
                    if (session.timeline) {
                        const patch = patchGenerator.generatePatch(
                            session.timeline,
                            data.issueStep,
                            data.fixType
                        );
                        ws.send(JSON.stringify({
                            type: 'PATCH_GENERATED',
                            patch
                        }));
                    }
                    break;

                case 'REPLAY_WITH_INPUTS':
                    if (session.timeline) {
                        const replayResult = await executionRecorder.replay(
                            session.timeline,
                            data.modifiedInputs
                        );
                        ws.send(JSON.stringify({
                            type: 'REPLAY_COMPLETE',
                            result: replayResult
                        }));
                    }
                    break;
            }
        } catch (error) {
            console.error('WebSocket error:', error);
            ws.send(JSON.stringify({
                type: 'ERROR',
                message: error.message
            }));
        }
    });

    ws.on('close', () => {
        sessions.delete(sessionId);
        console.log(`Session closed: ${sessionId}`);
    });
});

// REST API endpoints

// Record new execution trace
app.post('/api/record', async (req, res) => {
    try {
        const { programName, executionData } = req.body;
        const traceId = await executionRecorder.startRecording(programName);
        
        // Process execution data
        for (const step of executionData) {
            await executionRecorder.recordStep(traceId, step);
        }
        
        const timeline = await executionRecorder.finishRecording(traceId);
        res.json({ traceId, summary: timeline.getSummary() });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// List available traces
app.get('/api/traces', async (req, res) => {
    try {
        const traces = await timelineStorage.listTraces();
        res.json(traces);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Get trace details
app.get('/api/traces/:traceId', async (req, res) => {
    try {
        const timeline = await timelineStorage.loadTrace(req.params.traceId);
        res.json(timeline.getFullView());
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Search for specific patterns in traces
app.post('/api/search', async (req, res) => {
    try {
        const { pattern, searchType } = req.body;
        const results = await timelineStorage.searchTraces(pattern, searchType);
        res.json(results);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Compare two execution traces
app.post('/api/compare', async (req, res) => {
    try {
        const { traceId1, traceId2 } = req.body;
        const timeline1 = await timelineStorage.loadTrace(traceId1);
        const timeline2 = await timelineStorage.loadTrace(traceId2);
        
        const comparison = edgeCaseAnalyzer.compareExecutions(timeline1, timeline2);
        res.json(comparison);
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Export trace as various formats
app.get('/api/traces/:traceId/export', async (req, res) => {
    try {
        const { format = 'json' } = req.query;
        const timeline = await timelineStorage.loadTrace(req.params.traceId);
        
        let exportData;
        switch (format) {
            case 'json':
                exportData = timeline.toJSON();
                res.json(exportData);
                break;
            case 'csv':
                exportData = timeline.toCSV();
                res.setHeader('Content-Type', 'text/csv');
                res.send(exportData);
                break;
            case 'html':
                exportData = timeline.toHTML();
                res.setHeader('Content-Type', 'text/html');
                res.send(exportData);
                break;
            default:
                res.status(400).json({ error: 'Invalid format' });
        }
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Health check
app.get('/api/health', (req, res) => {
    res.json({
        status: 'healthy',
        uptime: process.uptime(),
        sessions: sessions.size,
        memory: process.memoryUsage()
    });
});