// Main Application Entry Point

class DebuggerApp {
    constructor() {
        this.timeline = null;
        this.variableInspector = null;
        this.currentTimeline = null;
        this.edgeCases = [];
        this.currentIssueStep = null;
        
        this.initializeComponents();
        this.attachEventListeners();
        this.setupWebSocketHandlers();
    }

    initializeComponents() {
        // Initialize timeline visualization
        this.timeline = new TimelineVisualization('timelineCanvas');
        
        // Initialize variable inspector
        this.variableInspector = new VariableInspector('variableInspector');
        
        // Initialize statistics chart
        initializeStatsChart();
        
        // Initialize replay engine scenarios
        window.replayEngine.addScenarioButtons();
    }

    attachEventListeners() {
        // Header buttons
        document.getElementById('loadTraceBtn').addEventListener('click', () => {
            loadAvailableTraces();
            showModal('loadTraceModal');
        });

        document.getElementById('recordBtn').addEventListener('click', () => {
            showNotification('Recording feature coming soon!', 'info');
        });

        document.getElementById('searchBtn').addEventListener('click', () => {
            showModal('searchModal');
        });

        // Timeline controls
        document.getElementById('playBtn').addEventListener('click', () => {
            this.timeline.play();
        });

        document.getElementById('pauseBtn').addEventListener('click', () => {
            this.timeline.pause();
        });

        document.getElementById('stepBackBtn').addEventListener('click', () => {
            this.timeline.stepBackward();
        });

        document.getElementById('stepForwardBtn').addEventListener('click', () => {
            this.timeline.stepForward();
        });

        document.getElementById('rewindBtn').addEventListener('click', () => {
            this.timeline.rewind();
        });

        document.getElementById('fastForwardBtn').addEventListener('click', () => {
            this.timeline.fastForward();
        });

        // Timeline slider
        const slider = document.getElementById('timelineSlider');
        slider.addEventListener('input', (e) => {
            const stepIndex = parseInt(e.target.value);
            this.timeline.goToStep(stepIndex);
        });

        // Custom timeline step change event
        window.addEventListener('timelineStepChange', (e) => {
            this.handleStepChange(e.detail.step);
        });

        // Edge case analysis
        document.getElementById('analyzeBtn').addEventListener('click', () => {
            this.analyzeEdgeCases();
        });

        // Patch generation
        document.getElementById('generatePatchBtn').addEventListener('click', () => {
            this.generatePatch();
        });

        // Search
        document.getElementById('performSearchBtn').addEventListener('click', () => {
            performSearch();
        });

        // Enter key in search input
        document.getElementById('searchInput').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') {
                performSearch();
            }
        });

        // Breakpoints
        document.getElementById('addBreakpointBtn').addEventListener('click', () => {
            const paragraph = prompt('Enter paragraph name for breakpoint:');
            if (paragraph) {
                window.wsManager.setBreakpoint(paragraph);
                showNotification(`Breakpoint set at ${paragraph}`, 'success');
            }
        });

        document.getElementById('clearBreakpointsBtn').addEventListener('click', () => {
            showNotification('Breakpoints cleared', 'info');
        });

        // Export buttons (add to UI if needed)
        document.addEventListener('keydown', (e) => {
            // Ctrl/Cmd + E for export
            if ((e.ctrlKey || e.metaKey) && e.key === 'e') {
                e.preventDefault();
                exportTrace('json');
            }
        });
    }

    setupWebSocketHandlers() {
        // Timeline loaded
        window.wsManager.on('TIMELINE_LOADED', (data) => {
            this.currentTimeline = data.timeline;
            window.currentTimeline = this.currentTimeline;
            
            // Create full timeline object for visualization
            const fullTimeline = {
                traceId: data.timeline.traceId,
                programName: data.timeline.programName,
                steps: data.timeline.summary.stepCount ? [] : data.timeline.steps,
                duration: data.timeline.duration,
                getSummary: () => data.timeline.summary,
                getSteps: () => data.timeline.steps || []
            };
            
            // Load timeline
            this.timeline.loadTimeline(fullTimeline);
            
            // Update UI
            document.getElementById('programName').textContent = data.timeline.programName;
            document.getElementById('stepIndicator').textContent = `Step: 0/${data.timeline.summary.stepCount}`;
            
            // Update slider
            const slider = document.getElementById('timelineSlider');
            slider.max = data.timeline.summary.stepCount - 1;
            slider.value = 0;
            
            // Load full timeline data
            this.loadFullTimeline(data.timeline.traceId);
            
            showNotification('Timeline loaded successfully', 'success');
        });

        // Step update
        window.wsManager.on('STEP_UPDATE', (data) => {
            this.handleStepUpdate(data.step, data.currentStep);
        });

        // Edge cases
        window.wsManager.on('EDGE_CASES', (data) => {
            this.displayEdgeCases(data.edgeCases);
        });

        // Patch generated
        window.wsManager.on('PATCH_GENERATED', (data) => {
            this.displayPatch(data.patch);
        });

        // Replay complete
        window.wsManager.on('REPLAY_COMPLETE', (data) => {
            window.replayEngine.displayReplayResults(data.result);
        });

        // Error handling
        window.wsManager.on('ERROR', (data) => {
            showNotification(`Error: ${data.message}`, 'error');
        });
    }

    async loadFullTimeline(traceId) {
        try {
            const response = await fetch(`/api/traces/${traceId}`);
            const fullData = await response.json();
            
            // Create proper Timeline object
            const timeline = {
                traceId: fullData.traceId,
                programName: fullData.programName,
                metadata: fullData.metadata,
                createdAt: fullData.createdAt,
                duration: fullData.duration,
                steps: fullData.steps,
                tags: fullData.tags,
                getSummary: function() {
                    return this.summary || {};
                },
                getSteps: function() {
                    return this.steps;
                },
                getCallGraph: function() {
                    // Implement if needed
                    return { nodes: [], edges: [] };
                }
            };
            
            // Update components with full timeline
            this.timeline.loadTimeline(timeline);
            window.replayEngine.setTimeline(timeline);
            
            // Update sidebar lists
            this.updateSidebarLists(timeline);
            
            // Update statistics
            const stats = this.timeline.getExecutionStats();
            updateStatsChart(stats);
            
        } catch (error) {
            console.error('Failed to load full timeline:', error);
            showNotification('Failed to load timeline details', 'error');
        }
    }

    handleStepChange(stepIndex) {
        // Update slider
        const slider = document.getElementById('timelineSlider');
        slider.value = stepIndex;
        
        // Update step indicator
        if (this.currentTimeline) {
            document.getElementById('stepIndicator').textContent = 
                `Step: ${stepIndex}/${this.currentTimeline.summary.stepCount}`;
        }
        
        // Request step details from server
        window.wsManager.goToStep(stepIndex);
    }

    handleStepUpdate(step, currentStep) {
        // Update variable inspector
        this.variableInspector.updateStep(step, this.currentTimeline);
        
        // Update code view
        updateCodeView(step);
        
        // Update execution path
        if (this.currentTimeline && this.currentTimeline.steps) {
            updateExecutionPath(this.currentTimeline.steps.slice(0, currentStep + 1));
        }
        
        // Update timeline position
        this.timeline.setCurrentStep(currentStep);
    }

    updateSidebarLists(timeline) {
        // Update recent traces
        const traceList = document.getElementById('traceList');
        traceList.innerHTML = '';
        
        const traceItem = createElement('div', 'trace-item active');
        traceItem.innerHTML = `
            <div>${timeline.programName}</div>
            <div class="trace-meta">${new Date(timeline.createdAt).toLocaleTimeString()}</div>
        `;
        traceList.appendChild(traceItem);
        
        // Update pattern list
        const patterns = [];
        timeline.steps.forEach(step => {
            if (step.patterns) {
                patterns.push(...step.patterns);
            }
        });
        updatePatternList(patterns);
        
        // Update bookmarks (if any)
        if (timeline.bookmarks) {
            updateBookmarkList(Array.from(timeline.bookmarks.entries()));
        }
    }

    analyzeEdgeCases() {
        if (!this.currentTimeline) {
            showNotification('No timeline loaded', 'warning');
            return;
        }
        
        window.wsManager.analyzeEdgeCases();
        showNotification('Analyzing edge cases...', 'info');
    }

    displayEdgeCases(edgeCases) {
        this.edgeCases = edgeCases;
        const resultsDiv = document.getElementById('edgeCaseResults');
        resultsDiv.innerHTML = '';
        
        if (!edgeCases.edgeCases || edgeCases.edgeCases.length === 0) {
            resultsDiv.innerHTML = '<p>No edge cases detected</p>';
            return;
        }
        
        // Group by severity
        const bySeverity = {};
        edgeCases.edgeCases.forEach(ec => {
            if (!bySeverity[ec.severity]) {
                bySeverity[ec.severity] = [];
            }
            bySeverity[ec.severity].push(ec);
        });
        
        // Display in severity order
        ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW', 'INFO'].forEach(severity => {
            if (bySeverity[severity]) {
                bySeverity[severity].forEach(ec => {
                    const item = createElement('div', `edge-case-item ${severity.toLowerCase()}`);
                    item.innerHTML = `
                        <div class="edge-case-type">${ec.type}</div>
                        <div class="edge-case-desc">${ec.description}</div>
                        <div class="edge-case-action">
                            <button class="btn btn-sm" onclick="debuggerApp.selectIssue(${ec.stepIndex || ec.worstCase?.stepIndex || 0})">
                                Go to Step
                            </button>
                        </div>
                    `;
                    resultsDiv.appendChild(item);
                });
            }
        });
        
        // Show summary
        showNotification(`Found ${edgeCases.edgeCases.length} edge cases`, 
                        edgeCases.summary.criticalIssues.length > 0 ? 'warning' : 'info');
    }

    selectIssue(stepIndex) {
        this.currentIssueStep = stepIndex;
        this.timeline.goToStep(stepIndex);
    }

    generatePatch() {
        if (this.currentIssueStep === null) {
            showNotification('Please select an issue first', 'warning');
            return;
        }
        
        const selectedPatchType = document.querySelector('input[name="patchType"]:checked');
        if (!selectedPatchType) {
            showNotification('Please select a patch type', 'warning');
            return;
        }
        
        window.wsManager.generatePatch(this.currentIssueStep, selectedPatchType.value);
        showNotification('Generating patch...', 'info');
    }

    displayPatch(patch) {
        const patchDiv = document.getElementById('patchResult');
        patchDiv.innerHTML = `
            <div class="patch-header">
                <strong>Patch for ${patch.targetParagraph}</strong>
                <button class="btn btn-sm" onclick="debuggerApp.copyPatch()">Copy</button>
            </div>
            <pre class="patch-code">${patch.patch}</pre>
            <div class="patch-explanation">
                <p><strong>Explanation:</strong> ${patch.explanation}</p>
                <p><strong>Impact:</strong> ${patch.impact.riskLevel} risk, affects ${patch.impact.affectedParagraphs.size} paragraphs</p>
            </div>
        `;
        
        // Store patch for copying
        this.currentPatch = patch.patch;
        
        showNotification('Patch generated successfully', 'success');
    }

    copyPatch() {
        if (this.currentPatch) {
            navigator.clipboard.writeText(this.currentPatch).then(() => {
                showNotification('Patch copied to clipboard', 'success');
            }).catch(() => {
                showNotification('Failed to copy patch', 'error');
            });
        }
    }

    // Keyboard shortcuts
    setupKeyboardShortcuts() {
        document.addEventListener('keydown', (e) => {
            // Don't trigger shortcuts when typing in inputs
            if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') {
                return;
            }
            
            switch(e.key) {
                case ' ':
                    e.preventDefault();
                    if (this.timeline.playing) {
                        this.timeline.pause();
                    } else {
                        this.timeline.play();
                    }
                    break;
                case 'ArrowLeft':
                    e.preventDefault();
                    this.timeline.stepBackward();
                    break;
                case 'ArrowRight':
                    e.preventDefault();
                    this.timeline.stepForward();
                    break;
                case 'Home':
                    e.preventDefault();
                    this.timeline.rewind();
                    break;
                case 'End':
                    e.preventDefault();
                    this.timeline.fastForward();
                    break;
            }
        });
    }
}

// Initialize app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    window.debuggerApp = new DebuggerApp();
    window.debuggerApp.setupKeyboardShortcuts();
    
    // Load example trace if available
    const urlParams = new URLSearchParams(window.location.search);
    const traceId = urlParams.get('trace');
    if (traceId) {
        setTimeout(() => {
            window.wsManager.loadTrace(traceId);
        }, 1000);
    }
    
    console.log('COBOL Time-Travel Debugger initialized! 🚀');
});