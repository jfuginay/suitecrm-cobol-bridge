class WebSocketManager {
    constructor() {
        this.ws = null;
        this.url = `ws://${window.location.hostname}:${window.location.port || 3005}`;
        this.reconnectInterval = 5000;
        this.messageHandlers = new Map();
        this.connectionStatus = document.getElementById('connectionStatus');
        
        this.connect();
    }

    connect() {
        try {
            this.ws = new WebSocket(this.url);
            
            this.ws.onopen = () => {
                console.log('WebSocket connected');
                this.updateConnectionStatus(true);
                this.onConnect();
            };

            this.ws.onmessage = (event) => {
                try {
                    const data = JSON.parse(event.data);
                    this.handleMessage(data);
                } catch (error) {
                    console.error('Error parsing WebSocket message:', error);
                }
            };

            this.ws.onerror = (error) => {
                console.error('WebSocket error:', error);
                this.updateConnectionStatus(false);
            };

            this.ws.onclose = () => {
                console.log('WebSocket disconnected');
                this.updateConnectionStatus(false);
                this.scheduleReconnect();
            };

        } catch (error) {
            console.error('Failed to create WebSocket:', error);
            this.scheduleReconnect();
        }
    }

    scheduleReconnect() {
        setTimeout(() => {
            console.log('Attempting to reconnect...');
            this.connect();
        }, this.reconnectInterval);
    }

    updateConnectionStatus(connected) {
        if (this.connectionStatus) {
            this.connectionStatus.classList.toggle('connected', connected);
        }
    }

    send(message) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(message));
        } else {
            console.error('WebSocket not connected');
        }
    }

    on(type, handler) {
        if (!this.messageHandlers.has(type)) {
            this.messageHandlers.set(type, []);
        }
        this.messageHandlers.get(type).push(handler);
    }

    handleMessage(data) {
        const handlers = this.messageHandlers.get(data.type);
        if (handlers) {
            handlers.forEach(handler => {
                try {
                    handler(data);
                } catch (error) {
                    console.error(`Error in handler for ${data.type}:`, error);
                }
            });
        }
    }

    onConnect() {
        // Override in subclass if needed
    }

    loadTrace(traceId) {
        this.send({
            type: 'LOAD_TRACE',
            traceId: traceId
        });
    }

    stepForward() {
        this.send({ type: 'STEP_FORWARD' });
    }

    stepBackward() {
        this.send({ type: 'STEP_BACKWARD' });
    }

    goToStep(stepIndex) {
        this.send({
            type: 'GOTO_STEP',
            stepIndex: stepIndex
        });
    }

    setBreakpoint(paragraph) {
        this.send({
            type: 'SET_BREAKPOINT',
            paragraph: paragraph
        });
    }

    analyzeEdgeCases() {
        this.send({ type: 'ANALYZE_EDGE_CASES' });
    }

    generatePatch(issueStep, fixType) {
        this.send({
            type: 'GENERATE_PATCH',
            issueStep: issueStep,
            fixType: fixType
        });
    }

    replayWithInputs(modifiedInputs) {
        this.send({
            type: 'REPLAY_WITH_INPUTS',
            modifiedInputs: modifiedInputs
        });
    }
}

// Create global instance
window.wsManager = new WebSocketManager();