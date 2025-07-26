class ReplayEngine {
    constructor() {
        this.originalTimeline = null;
        this.modifiedInputs = {};
        this.replayResults = null;
        this.inputCounter = 0;
        
        this.setupEventListeners();
    }

    setupEventListeners() {
        // Add input button
        document.getElementById('addInputBtn').addEventListener('click', () => {
            this.addInputField();
        });

        // Replay button
        document.getElementById('replayBtn').addEventListener('click', () => {
            this.startReplay();
        });

        // Handle input changes
        document.getElementById('inputModifications').addEventListener('change', (e) => {
            if (e.target.classList.contains('input-variable') || e.target.classList.contains('input-value')) {
                this.updateModifiedInputs();
            }
        });
    }

    setTimeline(timeline) {
        this.originalTimeline = timeline;
        this.resetInputs();
        this.populateInitialInputs();
    }

    resetInputs() {
        this.modifiedInputs = {};
        this.inputCounter = 0;
        document.getElementById('inputModifications').innerHTML = '';
        document.getElementById('replayResults').innerHTML = '';
    }

    populateInitialInputs() {
        if (!this.originalTimeline || !this.originalTimeline.steps.length) return;

        // Get variables from first step
        const firstStep = this.originalTimeline.steps[0];
        const variables = Object.entries(firstStep.variables)
            .filter(([name, info]) => {
                // Filter for likely input variables
                return name.startsWith('WS-') || 
                       name.startsWith('INPUT-') ||
                       name.includes('-IN') ||
                       info.type === 'NUMERIC' ||
                       info.type === 'ALPHANUMERIC';
            })
            .slice(0, 5); // Limit to first 5

        // Add input fields for each variable
        variables.forEach(([name, info]) => {
            this.addInputField(name, info.value);
        });
    }

    addInputField(varName = '', value = '') {
        const container = document.getElementById('inputModifications');
        const inputId = `input-${this.inputCounter++}`;
        
        const inputDiv = document.createElement('div');
        inputDiv.className = 'input-modification';
        inputDiv.id = inputId;
        inputDiv.innerHTML = `
            <input type="text" 
                   class="input-variable" 
                   placeholder="Variable name" 
                   value="${varName}">
            <input type="text" 
                   class="input-value" 
                   placeholder="New value" 
                   value="${value}">
            <button class="btn btn-sm btn-danger" onclick="window.replayEngine.removeInput('${inputId}')">×</button>
        `;
        
        container.appendChild(inputDiv);
    }

    removeInput(inputId) {
        const element = document.getElementById(inputId);
        if (element) {
            element.remove();
            this.updateModifiedInputs();
        }
    }

    updateModifiedInputs() {
        this.modifiedInputs = {};
        
        const inputs = document.querySelectorAll('.input-modification');
        inputs.forEach(inputDiv => {
            const varInput = inputDiv.querySelector('.input-variable');
            const valueInput = inputDiv.querySelector('.input-value');
            
            if (varInput.value && valueInput.value) {
                this.modifiedInputs[varInput.value] = valueInput.value;
            }
        });
    }

    async startReplay() {
        if (!this.originalTimeline) {
            alert('No timeline loaded');
            return;
        }

        this.updateModifiedInputs();
        
        if (Object.keys(this.modifiedInputs).length === 0) {
            alert('No input modifications specified');
            return;
        }

        // Show loading state
        const resultsDiv = document.getElementById('replayResults');
        resultsDiv.innerHTML = '<div class="loading">Running replay simulation...</div>';

        try {
            // Send replay request
            const response = await fetch('/api/replay', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    traceId: this.originalTimeline.traceId,
                    modifiedInputs: {
                        initialValues: this.modifiedInputs
                    }
                })
            });

            if (!response.ok) {
                throw new Error('Replay failed');
            }

            const result = await response.json();
            this.replayResults = result;
            this.displayReplayResults(result);
            
        } catch (error) {
            resultsDiv.innerHTML = `<div class="error">Replay error: ${error.message}</div>`;
        }
    }

    displayReplayResults(results) {
        const resultsDiv = document.getElementById('replayResults');
        
        let html = '<h4>Replay Results</h4>';
        
        // Summary
        html += `
            <div class="replay-summary">
                <p><strong>Replay ID:</strong> ${results.replayId}</p>
                <p><strong>Original Trace:</strong> ${results.original}</p>
                <p><strong>Differences Found:</strong> ${results.differences.length}</p>
            </div>
        `;

        // Modified inputs
        html += '<div class="modified-inputs-summary">';
        html += '<h5>Modified Inputs:</h5>';
        html += '<ul>';
        for (const [name, value] of Object.entries(results.modifiedInputs.initialValues || {})) {
            html += `<li>${name} = ${value}</li>`;
        }
        html += '</ul>';
        html += '</div>';

        // Differences
        if (results.differences.length > 0) {
            html += '<div class="replay-differences">';
            html += '<h5>Execution Differences:</h5>';
            
            results.differences.forEach(diff => {
                html += `
                    <div class="difference-item">
                        <p><strong>Step ${diff.stepIndex}:</strong></p>
                        <div class="diff-details">
                `;
                
                // Variable differences
                if (diff.differences.variables && Object.keys(diff.differences.variables).length > 0) {
                    html += '<p>Variable Changes:</p>';
                    html += '<table class="diff-table">';
                    html += '<tr><th>Variable</th><th>Original</th><th>Replayed</th></tr>';
                    
                    for (const [varName, values] of Object.entries(diff.differences.variables)) {
                        html += `
                            <tr>
                                <td>${varName}</td>
                                <td>${this.formatValue(values.original)}</td>
                                <td>${this.formatValue(values.replayed)}</td>
                            </tr>
                        `;
                    }
                    html += '</table>';
                }
                
                html += '</div></div>';
            });
            
            html += '</div>';
        } else {
            html += '<p class="no-differences">No differences found - execution would proceed identically</p>';
        }

        // Warnings and errors
        const warnings = this.analyzeReplayResults(results);
        if (warnings.length > 0) {
            html += '<div class="replay-warnings">';
            html += '<h5>Warnings:</h5>';
            html += '<ul>';
            warnings.forEach(warning => {
                html += `<li class="warning-item ${warning.severity}">${warning.message}</li>`;
            });
            html += '</ul>';
            html += '</div>';
        }

        resultsDiv.innerHTML = html;

        // Add export button
        const exportBtn = document.createElement('button');
        exportBtn.className = 'btn btn-secondary btn-sm';
        exportBtn.textContent = 'Export Results';
        exportBtn.onclick = () => this.exportReplayResults();
        resultsDiv.appendChild(exportBtn);
    }

    analyzeReplayResults(results) {
        const warnings = [];

        // Check for critical differences
        results.differences.forEach(diff => {
            const step = this.originalTimeline.steps[diff.stepIndex];
            
            // Check if differences occur in critical sections
            if (step.paragraph.includes('CALCULATE') || step.paragraph.includes('COMPUTE')) {
                warnings.push({
                    severity: 'high',
                    message: `Calculation differs at step ${diff.stepIndex} in ${step.paragraph}`
                });
            }

            // Check for file operation differences
            if (step.operation && (step.operation.includes('READ') || step.operation.includes('WRITE'))) {
                warnings.push({
                    severity: 'medium',
                    message: `File operation affected at step ${diff.stepIndex}`
                });
            }
        });

        // Check for potential issues with modified inputs
        for (const [name, value] of Object.entries(this.modifiedInputs)) {
            if (value === '0' && name.includes('DIVISOR')) {
                warnings.push({
                    severity: 'critical',
                    message: `Division by zero risk: ${name} set to 0`
                });
            }

            if (this.isNumeric(value) && Math.abs(parseFloat(value)) > 999999999) {
                warnings.push({
                    severity: 'high',
                    message: `Potential overflow: ${name} set to large value`
                });
            }
        }

        return warnings;
    }

    formatValue(value) {
        if (value === null || value === undefined) return '<null>';
        if (typeof value === 'string' && value.length > 50) {
            return value.substring(0, 47) + '...';
        }
        return value;
    }

    isNumeric(value) {
        return !isNaN(value) && !isNaN(parseFloat(value));
    }

    exportReplayResults() {
        if (!this.replayResults) return;

        const exportData = {
            timestamp: new Date().toISOString(),
            original: this.originalTimeline.toJSON(),
            replay: this.replayResults,
            analysis: this.analyzeReplayResults(this.replayResults)
        };

        const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `replay-${this.replayResults.replayId}.json`;
        a.click();
        URL.revokeObjectURL(url);
    }

    // Create replay scenarios
    createScenarios() {
        const scenarios = [
            {
                name: 'Boundary Test',
                description: 'Test with minimum and maximum values',
                inputs: this.generateBoundaryInputs()
            },
            {
                name: 'Null Test',
                description: 'Test with null/empty values',
                inputs: this.generateNullInputs()
            },
            {
                name: 'Overflow Test',
                description: 'Test with values that might cause overflow',
                inputs: this.generateOverflowInputs()
            },
            {
                name: 'Special Characters',
                description: 'Test with special characters in text fields',
                inputs: this.generateSpecialCharInputs()
            }
        ];

        return scenarios;
    }

    generateBoundaryInputs() {
        const inputs = {};
        
        if (this.originalTimeline && this.originalTimeline.steps.length > 0) {
            const firstStep = this.originalTimeline.steps[0];
            
            for (const [name, info] of Object.entries(firstStep.variables)) {
                if (info.type === 'NUMERIC') {
                    // Set to maximum value based on picture clause
                    const max = this.getMaxForPicture(info.picture);
                    inputs[name] = max.toString();
                }
            }
        }

        return inputs;
    }

    generateNullInputs() {
        const inputs = {};
        
        if (this.originalTimeline && this.originalTimeline.steps.length > 0) {
            const firstStep = this.originalTimeline.steps[0];
            
            for (const [name, info] of Object.entries(firstStep.variables)) {
                if (info.type === 'ALPHANUMERIC') {
                    inputs[name] = '';
                } else if (info.type === 'NUMERIC') {
                    inputs[name] = '0';
                }
            }
        }

        return inputs;
    }

    generateOverflowInputs() {
        const inputs = {};
        
        if (this.originalTimeline && this.originalTimeline.steps.length > 0) {
            const firstStep = this.originalTimeline.steps[0];
            
            for (const [name, info] of Object.entries(firstStep.variables)) {
                if (info.type === 'NUMERIC') {
                    // Set to value larger than picture allows
                    const max = this.getMaxForPicture(info.picture);
                    inputs[name] = (max * 10).toString();
                }
            }
        }

        return inputs;
    }

    generateSpecialCharInputs() {
        const inputs = {};
        const specialChars = ['!@#$%', '\\n\\r\\t', '""\'\'', '><&'];
        
        if (this.originalTimeline && this.originalTimeline.steps.length > 0) {
            const firstStep = this.originalTimeline.steps[0];
            let charIndex = 0;
            
            for (const [name, info] of Object.entries(firstStep.variables)) {
                if (info.type === 'ALPHANUMERIC') {
                    inputs[name] = specialChars[charIndex % specialChars.length];
                    charIndex++;
                }
            }
        }

        return inputs;
    }

    getMaxForPicture(picture) {
        if (!picture) return 999999999;
        
        const match = picture.match(/9\((\d+)\)/);
        if (match) {
            const digits = parseInt(match[1]);
            return Math.pow(10, digits) - 1;
        }
        
        return 999999999;
    }

    // Quick scenario buttons
    addScenarioButtons() {
        const container = document.createElement('div');
        container.className = 'scenario-buttons';
        container.innerHTML = '<h5>Quick Scenarios:</h5>';

        const scenarios = this.createScenarios();
        
        scenarios.forEach(scenario => {
            const btn = document.createElement('button');
            btn.className = 'btn btn-sm btn-info';
            btn.textContent = scenario.name;
            btn.title = scenario.description;
            btn.onclick = () => this.applyScenario(scenario);
            container.appendChild(btn);
        });

        const replaySection = document.querySelector('.replay-inputs');
        replaySection.insertBefore(container, replaySection.firstChild);
    }

    applyScenario(scenario) {
        // Clear existing inputs
        this.resetInputs();
        
        // Apply scenario inputs
        for (const [name, value] of Object.entries(scenario.inputs)) {
            this.addInputField(name, value);
        }
        
        this.updateModifiedInputs();
    }
}

// Create global instance
window.replayEngine = new ReplayEngine();