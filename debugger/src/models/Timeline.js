class Timeline {
    constructor(traceId, programName, metadata = {}) {
        this.traceId = traceId;
        this.programName = programName;
        this.metadata = metadata;
        this.createdAt = Date.now();
        this.steps = [];
        this.duration = 0;
        this.tags = [];
        this.bookmarks = new Map();
        this.annotations = new Map();
    }

    addStep(step) {
        this.steps.push(step);
        
        // Update duration
        if (step.timestamp > this.duration) {
            this.duration = step.timestamp;
        }

        // Auto-tag interesting steps
        if (step.patterns && step.patterns.length > 0) {
            this.addTag(`pattern:${step.patterns[0].type}`);
        }
    }

    getStep(index) {
        return this.steps[index] || null;
    }

    getSteps() {
        return this.steps;
    }

    stepForward(currentIndex) {
        if (currentIndex < this.steps.length - 1) {
            return this.steps[currentIndex + 1];
        }
        return null;
    }

    stepBackward(currentIndex) {
        if (currentIndex > 0) {
            return this.steps[currentIndex - 1];
        }
        return null;
    }

    getRecentSteps(count) {
        return this.steps.slice(-count);
    }

    findStepsByParagraph(paragraph) {
        return this.steps.filter(step => step.paragraph === paragraph);
    }

    findStepsByPattern(patternType) {
        return this.steps.filter(step => 
            step.patterns.some(p => p.type === patternType)
        );
    }

    getVariableHistory(variableName) {
        const history = [];
        
        for (const step of this.steps) {
            if (step.variables[variableName]) {
                history.push({
                    stepIndex: step.index,
                    timestamp: step.timestamp,
                    value: step.variables[variableName],
                    paragraph: step.paragraph
                });
            }
        }

        return history;
    }

    getExecutionPath() {
        const path = [];
        let currentSection = null;

        for (const step of this.steps) {
            if (step.section !== currentSection) {
                currentSection = step.section;
                path.push({
                    section: currentSection,
                    startStep: step.index,
                    timestamp: step.timestamp
                });
            }
        }

        return path;
    }

    getCallGraph() {
        const graph = {
            nodes: new Set(),
            edges: []
        };

        for (let i = 0; i < this.steps.length - 1; i++) {
            const from = this.steps[i].paragraph;
            const to = this.steps[i + 1].paragraph;
            
            graph.nodes.add(from);
            graph.nodes.add(to);
            
            if (from !== to) {
                graph.edges.push({ from, to, step: i });
            }
        }

        return {
            nodes: Array.from(graph.nodes),
            edges: graph.edges
        };
    }

    addBookmark(stepIndex, label, description = '') {
        this.bookmarks.set(stepIndex, {
            label,
            description,
            createdAt: Date.now()
        });
    }

    addAnnotation(stepIndex, text, type = 'note') {
        if (!this.annotations.has(stepIndex)) {
            this.annotations.set(stepIndex, []);
        }
        
        this.annotations.get(stepIndex).push({
            text,
            type,
            createdAt: Date.now()
        });
    }

    addTag(tag) {
        if (!this.tags.includes(tag)) {
            this.tags.push(tag);
        }
    }

    finalize(stats) {
        this.duration = stats.duration;
        this.finalized = true;
        this.finalizedAt = Date.now();
    }

    getSummary() {
        const patternCounts = {};
        const paragraphCounts = {};

        for (const step of this.steps) {
            // Count paragraph executions
            paragraphCounts[step.paragraph] = (paragraphCounts[step.paragraph] || 0) + 1;

            // Count patterns
            for (const pattern of step.patterns || []) {
                patternCounts[pattern.type] = (patternCounts[pattern.type] || 0) + 1;
            }
        }

        return {
            traceId: this.traceId,
            programName: this.programName,
            createdAt: this.createdAt,
            duration: this.duration,
            stepCount: this.steps.length,
            paragraphCount: Object.keys(paragraphCounts).length,
            patternCounts,
            paragraphCounts,
            tags: this.tags,
            hasErrors: Object.keys(patternCounts).some(p => 
                ['DIVISION_BY_ZERO_RISK', 'FILE_ERROR', 'INFINITE_LOOP'].includes(p)
            )
        };
    }

    getCompactView() {
        return {
            traceId: this.traceId,
            programName: this.programName,
            metadata: this.metadata,
            stepCount: this.steps.length,
            duration: this.duration,
            summary: this.getSummary()
        };
    }

    getFullView() {
        return {
            ...this.toJSON(),
            bookmarks: Array.from(this.bookmarks.entries()),
            annotations: Array.from(this.annotations.entries())
        };
    }

    clone() {
        const cloned = new Timeline(this.traceId + '-clone', this.programName, { ...this.metadata });
        cloned.steps = this.steps.map(step => ({ ...step }));
        cloned.duration = this.duration;
        cloned.tags = [...this.tags];
        return cloned;
    }

    applyInputModifications(modifications) {
        // Apply modifications to initial variable states
        if (modifications.initialValues) {
            for (const step of this.steps) {
                if (step.index === 0) {
                    step.variables = {
                        ...step.variables,
                        ...modifications.initialValues
                    };
                }
            }
        }

        // Apply step-specific modifications
        if (modifications.stepModifications) {
            for (const [stepIndex, mods] of Object.entries(modifications.stepModifications)) {
                const step = this.steps[parseInt(stepIndex)];
                if (step) {
                    step.variables = {
                        ...step.variables,
                        ...mods
                    };
                }
            }
        }
    }

    toJSON() {
        return {
            traceId: this.traceId,
            programName: this.programName,
            metadata: this.metadata,
            createdAt: this.createdAt,
            duration: this.duration,
            steps: this.steps,
            tags: this.tags
        };
    }

    toCSV() {
        const headers = [
            'Step', 'Timestamp', 'Paragraph', 'Section', 'Operation',
            'Variables', 'Patterns', 'Conditions'
        ];

        const rows = [headers.join(',')];

        for (const step of this.steps) {
            const row = [
                step.index,
                step.timestamp,
                step.paragraph,
                step.section || '',
                step.operation || '',
                JSON.stringify(step.variables),
                JSON.stringify(step.patterns || []),
                JSON.stringify(step.conditions || {})
            ];
            rows.push(row.map(v => `"${v}"`).join(','));
        }

        return rows.join('\n');
    }

    toHTML() {
        return `
            <!DOCTYPE html>
            <html>
            <head>
                <title>Timeline: ${this.programName} - ${this.traceId}</title>
                <style>
                    body { font-family: 'Courier New', monospace; margin: 20px; }
                    .header { background: #333; color: white; padding: 10px; }
                    .step { margin: 10px 0; padding: 10px; border-left: 3px solid #007bff; }
                    .step:hover { background: #f0f0f0; }
                    .pattern { color: red; font-weight: bold; }
                    .variable { color: blue; }
                    .timestamp { color: #666; font-size: 0.9em; }
                </style>
            </head>
            <body>
                <div class="header">
                    <h1>${this.programName}</h1>
                    <p>Trace ID: ${this.traceId}</p>
                    <p>Duration: ${this.duration}ms | Steps: ${this.steps.length}</p>
                </div>
                <div class="timeline">
                    ${this.steps.map(step => `
                        <div class="step">
                            <span class="timestamp">[${step.timestamp}ms]</span>
                            <strong>Step ${step.index}:</strong> ${step.paragraph}
                            ${step.patterns.length > 0 ? 
                                `<span class="pattern">${step.patterns.map(p => p.type).join(', ')}</span>` : 
                                ''}
                            <div class="variables">
                                ${Object.entries(step.variables).slice(0, 3).map(([k, v]) => 
                                    `<span class="variable">${k}=${JSON.stringify(v.value)}</span>`
                                ).join(' | ')}
                            </div>
                        </div>
                    `).join('')}
                </div>
            </body>
            </html>
        `;
    }

    static fromJSON(data) {
        const timeline = new Timeline(data.traceId, data.programName, data.metadata);
        timeline.createdAt = data.createdAt;
        timeline.duration = data.duration;
        timeline.steps = data.steps;
        timeline.tags = data.tags || [];
        return timeline;
    }
}

module.exports = Timeline;