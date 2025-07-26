class VariableInspector {
    constructor(containerId) {
        this.container = document.getElementById(containerId);
        this.tableBody = document.getElementById('variableTableBody');
        this.searchInput = document.getElementById('variableSearch');
        this.currentStep = null;
        this.previousStep = null;
        this.watchedVariables = new Set();
        this.variableHistory = new Map();
        
        this.setupEventListeners();
    }

    setupEventListeners() {
        // Search functionality
        this.searchInput.addEventListener('input', (e) => {
            this.filterVariables(e.target.value);
        });

        // Watch button
        document.getElementById('watchBtn').addEventListener('click', () => {
            this.addSelectedToWatch();
        });

        // Variable row clicks
        this.tableBody.addEventListener('click', (e) => {
            const row = e.target.closest('tr');
            if (row) {
                row.classList.toggle('selected');
            }
        });

        // Double-click to view history
        this.tableBody.addEventListener('dblclick', (e) => {
            const row = e.target.closest('tr');
            if (row) {
                const varName = row.dataset.variable;
                this.showVariableHistory(varName);
            }
        });
    }

    updateStep(step, timeline) {
        this.previousStep = this.currentStep;
        this.currentStep = step;
        
        // Update variable history
        this.updateVariableHistory(step);
        
        // Render variables
        this.renderVariables();
        
        // Update statistics
        this.updateStatistics();
    }

    updateVariableHistory(step) {
        for (const [name, info] of Object.entries(step.variables)) {
            if (!this.variableHistory.has(name)) {
                this.variableHistory.set(name, []);
            }
            
            const history = this.variableHistory.get(name);
            const lastEntry = history[history.length - 1];
            
            // Only add if value changed
            if (!lastEntry || lastEntry.value !== info.value) {
                history.push({
                    stepIndex: step.index,
                    timestamp: step.timestamp,
                    value: info.value,
                    type: info.type,
                    paragraph: step.paragraph
                });
            }
        }
    }

    renderVariables() {
        this.tableBody.innerHTML = '';
        
        if (!this.currentStep) {
            this.tableBody.innerHTML = '<tr><td colspan="5">No step selected</td></tr>';
            return;
        }

        const variables = this.currentStep.variables;
        const sortedVars = Object.entries(variables).sort(([a], [b]) => a.localeCompare(b));

        for (const [name, info] of sortedVars) {
            const row = this.createVariableRow(name, info);
            this.tableBody.appendChild(row);
        }
    }

    createVariableRow(name, info) {
        const row = document.createElement('tr');
        row.dataset.variable = name;
        
        // Check if value changed
        const changed = this.hasVariableChanged(name, info);
        if (changed) {
            row.classList.add('variable-changed');
        }

        // Check if watched
        if (this.watchedVariables.has(name)) {
            row.classList.add('watched');
        }

        // Name cell
        const nameCell = document.createElement('td');
        nameCell.textContent = name;
        if (this.watchedVariables.has(name)) {
            nameCell.innerHTML = `⭐ ${name}`;
        }
        row.appendChild(nameCell);

        // Value cell
        const valueCell = document.createElement('td');
        valueCell.className = 'variable-value';
        valueCell.textContent = this.formatValue(info.value, info.type);
        row.appendChild(valueCell);

        // Type cell
        const typeCell = document.createElement('td');
        typeCell.textContent = info.type;
        row.appendChild(typeCell);

        // Picture cell
        const pictureCell = document.createElement('td');
        pictureCell.textContent = info.picture || '-';
        pictureCell.className = 'picture-clause';
        row.appendChild(pictureCell);

        // Changes cell
        const changesCell = document.createElement('td');
        const changeCount = this.getChangeCount(name);
        changesCell.textContent = changeCount;
        if (changeCount > 5) {
            changesCell.style.color = '#ff6b6b';
            changesCell.title = 'High change frequency';
        }
        row.appendChild(changesCell);

        return row;
    }

    formatValue(value, type) {
        if (value === null || value === undefined) {
            return '<null>';
        }

        switch (type) {
            case 'NUMERIC':
            case 'NUMERIC-STRING':
                // Format numbers with thousand separators
                return value.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
            
            case 'DECIMAL':
                // Format decimals
                const num = parseFloat(value);
                return num.toFixed(2).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
            
            case 'ALPHANUMERIC':
                // Show strings with quotes and escape special chars
                const str = value.toString();
                if (str.length > 50) {
                    return `"${str.substring(0, 47)}..."`;
                }
                return `"${str.replace(/"/g, '\\"')}"`;
            
            default:
                return value.toString();
        }
    }

    hasVariableChanged(name, info) {
        if (!this.previousStep) return false;
        
        const prevValue = this.previousStep.variables[name];
        if (!prevValue) return true; // New variable
        
        return prevValue.value !== info.value;
    }

    getChangeCount(name) {
        const history = this.variableHistory.get(name);
        return history ? history.length - 1 : 0;
    }

    filterVariables(searchTerm) {
        const rows = this.tableBody.querySelectorAll('tr');
        const term = searchTerm.toLowerCase();

        rows.forEach(row => {
            const varName = row.dataset.variable;
            if (!varName) return;

            const matches = varName.toLowerCase().includes(term) ||
                           row.textContent.toLowerCase().includes(term);
            
            row.style.display = matches ? '' : 'none';
        });
    }

    addSelectedToWatch() {
        const selected = this.tableBody.querySelectorAll('tr.selected');
        
        selected.forEach(row => {
            const varName = row.dataset.variable;
            if (varName) {
                this.watchedVariables.add(varName);
            }
        });

        this.renderVariables();
        this.updateWatchList();
    }

    updateWatchList() {
        // Update a separate watch list UI if needed
        const watchCount = this.watchedVariables.size;
        console.log(`Watching ${watchCount} variables`);
    }

    showVariableHistory(varName) {
        const history = this.variableHistory.get(varName);
        if (!history || history.length === 0) {
            alert(`No history for variable: ${varName}`);
            return;
        }

        // Create history modal
        const modal = document.createElement('div');
        modal.className = 'variable-history-modal';
        modal.innerHTML = `
            <div class="modal-content">
                <h3>History: ${varName}</h3>
                <div class="history-chart">
                    <canvas id="historyChart"></canvas>
                </div>
                <div class="history-table">
                    <table>
                        <thead>
                            <tr>
                                <th>Step</th>
                                <th>Time</th>
                                <th>Value</th>
                                <th>Paragraph</th>
                            </tr>
                        </thead>
                        <tbody>
                            ${history.map(h => `
                                <tr>
                                    <td>${h.stepIndex}</td>
                                    <td>${h.timestamp}ms</td>
                                    <td>${this.formatValue(h.value, h.type)}</td>
                                    <td>${h.paragraph}</td>
                                </tr>
                            `).join('')}
                        </tbody>
                    </table>
                </div>
                <button onclick="this.parentElement.parentElement.remove()">Close</button>
            </div>
        `;

        document.body.appendChild(modal);

        // Draw history chart if numeric
        if (history[0].type === 'NUMERIC' || history[0].type === 'DECIMAL') {
            this.drawHistoryChart('historyChart', varName, history);
        }
    }

    drawHistoryChart(canvasId, varName, history) {
        const canvas = document.getElementById(canvasId);
        const ctx = canvas.getContext('2d');
        
        // Prepare data
        const labels = history.map(h => h.stepIndex);
        const data = history.map(h => parseFloat(h.value));

        new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: [{
                    label: varName,
                    data: data,
                    borderColor: '#007bff',
                    backgroundColor: 'rgba(0, 123, 255, 0.1)',
                    tension: 0.1
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    title: {
                        display: true,
                        text: `Value changes for ${varName}`
                    }
                },
                scales: {
                    x: {
                        title: {
                            display: true,
                            text: 'Step'
                        }
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Value'
                        }
                    }
                }
            }
        });
    }

    updateStatistics() {
        if (!this.currentStep) return;

        const stats = {
            totalVars: Object.keys(this.currentStep.variables).length,
            numericVars: 0,
            alphanumericVars: 0,
            changedVars: 0,
            totalMemory: 0
        };

        for (const [name, info] of Object.entries(this.currentStep.variables)) {
            if (info.type === 'NUMERIC' || info.type === 'DECIMAL') {
                stats.numericVars++;
            } else {
                stats.alphanumericVars++;
            }

            if (this.hasVariableChanged(name, info)) {
                stats.changedVars++;
            }

            stats.totalMemory += info.size || 0;
        }

        // Update UI with statistics
        this.displayStatistics(stats);
    }

    displayStatistics(stats) {
        // Find or create stats display
        let statsDiv = document.getElementById('variableStats');
        if (!statsDiv) {
            statsDiv = document.createElement('div');
            statsDiv.id = 'variableStats';
            statsDiv.className = 'variable-stats';
            this.container.parentElement.insertBefore(statsDiv, this.container);
        }

        statsDiv.innerHTML = `
            <div class="stat-item">
                <span class="stat-label">Total:</span>
                <span class="stat-value">${stats.totalVars}</span>
            </div>
            <div class="stat-item">
                <span class="stat-label">Numeric:</span>
                <span class="stat-value">${stats.numericVars}</span>
            </div>
            <div class="stat-item">
                <span class="stat-label">Text:</span>
                <span class="stat-value">${stats.alphanumericVars}</span>
            </div>
            <div class="stat-item">
                <span class="stat-label">Changed:</span>
                <span class="stat-value">${stats.changedVars}</span>
            </div>
            <div class="stat-item">
                <span class="stat-label">Memory:</span>
                <span class="stat-value">${stats.totalMemory} bytes</span>
            </div>
        `;
    }

    exportVariables() {
        if (!this.currentStep) return;

        const data = {
            stepIndex: this.currentStep.index,
            timestamp: this.currentStep.timestamp,
            variables: this.currentStep.variables,
            history: Object.fromEntries(this.variableHistory)
        };

        const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `variables-step-${this.currentStep.index}.json`;
        a.click();
        URL.revokeObjectURL(url);
    }

    compareSteps(step1, step2) {
        const diff = {
            added: [],
            removed: [],
            modified: []
        };

        const vars1 = step1.variables;
        const vars2 = step2.variables;

        // Check for added/modified
        for (const [name, info2] of Object.entries(vars2)) {
            if (!vars1[name]) {
                diff.added.push({ name, value: info2.value });
            } else if (vars1[name].value !== info2.value) {
                diff.modified.push({
                    name,
                    oldValue: vars1[name].value,
                    newValue: info2.value
                });
            }
        }

        // Check for removed
        for (const name of Object.keys(vars1)) {
            if (!vars2[name]) {
                diff.removed.push({ name, value: vars1[name].value });
            }
        }

        return diff;
    }

    highlightDifferences(step1, step2) {
        const diff = this.compareSteps(step1, step2);
        
        // Apply highlighting
        const rows = this.tableBody.querySelectorAll('tr');
        rows.forEach(row => {
            const varName = row.dataset.variable;
            
            if (diff.added.some(v => v.name === varName)) {
                row.classList.add('diff-added');
            } else if (diff.removed.some(v => v.name === varName)) {
                row.classList.add('diff-removed');
            } else if (diff.modified.some(v => v.name === varName)) {
                row.classList.add('diff-modified');
            }
        });
    }
}

// Export for use in other modules
window.VariableInspector = VariableInspector;