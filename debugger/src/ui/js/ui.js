// UI Helper Functions and Event Handlers

// Modal functions
function showModal(modalId) {
    const modal = document.getElementById(modalId);
    if (modal) {
        modal.classList.add('active');
    }
}

function closeModal(modalId) {
    const modal = document.getElementById(modalId);
    if (modal) {
        modal.classList.remove('active');
    }
}

// Close modal when clicking outside
document.addEventListener('click', (e) => {
    if (e.target.classList.contains('modal')) {
        e.target.classList.remove('active');
    }
});

// Format timestamp
function formatTimestamp(ms) {
    if (ms < 1000) {
        return `${ms}ms`;
    }
    return `${(ms / 1000).toFixed(2)}s`;
}

// Format file size
function formatSize(bytes) {
    const units = ['B', 'KB', 'MB', 'GB'];
    let size = bytes;
    let unitIndex = 0;
    
    while (size >= 1024 && unitIndex < units.length - 1) {
        size /= 1024;
        unitIndex++;
    }
    
    return `${size.toFixed(2)} ${units[unitIndex]}`;
}

// Create element helper
function createElement(tag, className, content) {
    const element = document.createElement(tag);
    if (className) element.className = className;
    if (content) element.textContent = content;
    return element;
}

// Show notification
function showNotification(message, type = 'info') {
    // Create notification element
    const notification = createElement('div', `notification notification-${type}`);
    notification.textContent = message;
    
    // Add to page
    document.body.appendChild(notification);
    
    // Animate in
    setTimeout(() => notification.classList.add('show'), 10);
    
    // Remove after 3 seconds
    setTimeout(() => {
        notification.classList.remove('show');
        setTimeout(() => notification.remove(), 300);
    }, 3000);
}

// Update execution path display
function updateExecutionPath(steps) {
    const pathDiv = document.getElementById('executionPath');
    pathDiv.innerHTML = '';
    
    const recentSteps = steps.slice(-10); // Show last 10 steps
    recentSteps.forEach((step, index) => {
        const stepSpan = createElement('span', 'path-step', step.paragraph);
        if (index === recentSteps.length - 1) {
            stepSpan.classList.add('current');
        }
        pathDiv.appendChild(stepSpan);
        
        if (index < recentSteps.length - 1) {
            pathDiv.appendChild(createElement('span', 'path-arrow', ' → '));
        }
    });
}

// Update code view
function updateCodeView(step) {
    const codeContent = document.getElementById('codeContent');
    
    // Simulate COBOL code display
    const code = generateCobolSnippet(step);
    codeContent.innerHTML = '';
    
    code.split('\n').forEach((line, index) => {
        const lineNum = step.line - 5 + index;
        const lineDiv = createElement('div', 'code-line');
        
        if (lineNum === step.line) {
            lineDiv.classList.add('current');
        }
        
        lineDiv.innerHTML = `<span class="line-number">${lineNum.toString().padStart(6, '0')}</span> ${escapeHtml(line)}`;
        codeContent.appendChild(lineDiv);
    });
}

// Generate COBOL snippet for display
function generateCobolSnippet(step) {
    // This would normally load from actual COBOL source
    const snippets = {
        'MAIN-PROCEDURE': `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-INTEREST.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM VALIDATE-INPUT
           PERFORM CALCULATE-INTEREST
           PERFORM WRITE-OUTPUT
           STOP RUN.`,
        
        'CALCULATE-INTEREST': `       CALCULATE-INTEREST.
           COMPUTE WS-MONTHLY-RATE = WS-RATE / 12 / 100
           COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-MONTHLY-RATE * WS-TERM
           IF WS-DIVISOR NOT = ZERO
               DIVIDE WS-INTEREST BY WS-DIVISOR GIVING WS-RESULT
           ELSE
               DISPLAY "ERROR: Division by zero"
           END-IF.`,
        
        'VALIDATE-INPUT': `       VALIDATE-INPUT.
           IF WS-PRINCIPAL <= ZERO
               MOVE "Y" TO WS-ERROR-FLAG
               DISPLAY "ERROR: Invalid principal amount"
           END-IF
           IF WS-RATE <= ZERO OR WS-RATE > 100
               MOVE "Y" TO WS-ERROR-FLAG
               DISPLAY "ERROR: Invalid interest rate"
           END-IF.`
    };
    
    return snippets[step.paragraph] || `       ${step.paragraph}.\n           * Code for ${step.paragraph}`;
}

// Escape HTML
function escapeHtml(text) {
    const map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
    };
    return text.replace(/[&<>"']/g, m => map[m]);
}

// Update pattern list
function updatePatternList(patterns) {
    const patternList = document.getElementById('patternList');
    patternList.innerHTML = '';
    
    const patternCounts = {};
    patterns.forEach(pattern => {
        patternCounts[pattern.type] = (patternCounts[pattern.type] || 0) + 1;
    });
    
    Object.entries(patternCounts).forEach(([type, count]) => {
        const item = createElement('div', 'pattern-item');
        item.innerHTML = `
            <span class="pattern-type">${type}</span>
            <span class="pattern-count">${count}</span>
        `;
        item.onclick = () => filterByPattern(type);
        patternList.appendChild(item);
    });
}

// Filter timeline by pattern
function filterByPattern(patternType) {
    console.log(`Filtering by pattern: ${patternType}`);
    // Implementation would filter the timeline view
}

// Update bookmarks
function updateBookmarkList(bookmarks) {
    const bookmarkList = document.getElementById('bookmarkList');
    bookmarkList.innerHTML = '';
    
    bookmarks.forEach(([stepIndex, bookmark]) => {
        const item = createElement('div', 'bookmark-item');
        item.innerHTML = `
            <span class="bookmark-label">${bookmark.label}</span>
            <span class="bookmark-step">Step ${stepIndex}</span>
        `;
        item.onclick = () => window.timeline.goToStep(stepIndex);
        bookmarkList.appendChild(item);
    });
}

// Load available traces
async function loadAvailableTraces() {
    try {
        const response = await fetch('/api/traces');
        const traces = await response.json();
        
        const traceListDiv = document.getElementById('availableTraces');
        traceListDiv.innerHTML = '';
        
        traces.forEach(trace => {
            const traceDiv = createElement('div', 'trace-item');
            traceDiv.innerHTML = `
                <div class="trace-name">${trace.program_name}</div>
                <div class="trace-info">
                    <span>${new Date(trace.created_at).toLocaleString()}</span>
                    <span>${trace.step_count} steps</span>
                    <span>${formatTimestamp(trace.duration)}</span>
                </div>
            `;
            traceDiv.onclick = () => {
                window.wsManager.loadTrace(trace.trace_id);
                closeModal('loadTraceModal');
            };
            traceListDiv.appendChild(traceDiv);
        });
        
    } catch (error) {
        console.error('Failed to load traces:', error);
        showNotification('Failed to load traces', 'error');
    }
}

// Perform search
async function performSearch() {
    const searchInput = document.getElementById('searchInput');
    const searchType = document.getElementById('searchType');
    const resultsDiv = document.getElementById('searchResults');
    
    if (!searchInput.value) {
        showNotification('Please enter a search term', 'warning');
        return;
    }
    
    resultsDiv.innerHTML = '<div class="loading">Searching...</div>';
    
    try {
        const response = await fetch('/api/search', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                pattern: searchInput.value,
                searchType: searchType.value
            })
        });
        
        const results = await response.json();
        resultsDiv.innerHTML = '';
        
        if (results.length === 0) {
            resultsDiv.innerHTML = '<p>No results found</p>';
            return;
        }
        
        results.forEach(result => {
            const resultDiv = createElement('div', 'search-result-item');
            resultDiv.innerHTML = `
                <div>Trace: ${result.trace_id.substring(0, 8)}...</div>
                <div>Program: ${result.program_name}</div>
                <div>Step ${result.step_index}: ${result.paragraph}</div>
            `;
            resultDiv.onclick = () => {
                window.wsManager.loadTrace(result.trace_id);
                setTimeout(() => {
                    window.timeline.goToStep(result.step_index);
                }, 500);
                closeModal('searchModal');
            };
            resultsDiv.appendChild(resultDiv);
        });
        
    } catch (error) {
        console.error('Search failed:', error);
        resultsDiv.innerHTML = '<p class="error">Search failed</p>';
    }
}

// Export current trace
async function exportTrace(format = 'json') {
    if (!window.currentTimeline) {
        showNotification('No trace loaded', 'warning');
        return;
    }
    
    try {
        const response = await fetch(`/api/traces/${window.currentTimeline.traceId}/export?format=${format}`);
        
        if (format === 'json') {
            const data = await response.json();
            const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
            downloadBlob(blob, `trace-${window.currentTimeline.traceId}.json`);
        } else if (format === 'csv') {
            const text = await response.text();
            const blob = new Blob([text], { type: 'text/csv' });
            downloadBlob(blob, `trace-${window.currentTimeline.traceId}.csv`);
        } else if (format === 'html') {
            const html = await response.text();
            const blob = new Blob([html], { type: 'text/html' });
            downloadBlob(blob, `trace-${window.currentTimeline.traceId}.html`);
        }
        
        showNotification('Export successful', 'success');
    } catch (error) {
        console.error('Export failed:', error);
        showNotification('Export failed', 'error');
    }
}

// Download blob helper
function downloadBlob(blob, filename) {
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
}

// Initialize statistics chart
function initializeStatsChart() {
    const ctx = document.getElementById('statsChart').getContext('2d');
    window.statsChart = new Chart(ctx, {
        type: 'doughnut',
        data: {
            labels: [],
            datasets: [{
                data: [],
                backgroundColor: [
                    '#FF6384',
                    '#36A2EB',
                    '#FFCE56',
                    '#4BC0C0',
                    '#9966FF'
                ]
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'bottom',
                    labels: {
                        font: {
                            size: 10
                        }
                    }
                }
            }
        }
    });
}

// Update statistics chart
function updateStatsChart(statistics) {
    if (!window.statsChart) return;
    
    const labels = Object.keys(statistics.paragraphExecutions).slice(0, 5);
    const data = labels.map(label => statistics.paragraphExecutions[label]);
    
    window.statsChart.data.labels = labels;
    window.statsChart.data.datasets[0].data = data;
    window.statsChart.update();
}

// Add CSS for notifications
const style = document.createElement('style');
style.textContent = `
.notification {
    position: fixed;
    top: 20px;
    right: 20px;
    padding: 1rem 1.5rem;
    border-radius: 4px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    opacity: 0;
    transform: translateX(100%);
    transition: all 0.3s ease;
    z-index: 10000;
}

.notification.show {
    opacity: 1;
    transform: translateX(0);
}

.notification-info {
    background-color: #17a2b8;
    color: white;
}

.notification-success {
    background-color: #28a745;
    color: white;
}

.notification-warning {
    background-color: #ffc107;
    color: #212529;
}

.notification-error {
    background-color: #dc3545;
    color: white;
}

.path-step {
    display: inline-block;
    padding: 0.25rem 0.5rem;
    background-color: #e9ecef;
    border-radius: 4px;
    font-size: 0.875rem;
    margin: 0 0.25rem;
}

.path-step.current {
    background-color: #007bff;
    color: white;
}

.path-arrow {
    color: #6c757d;
}

.line-number {
    color: #6c757d;
    margin-right: 1rem;
    font-size: 0.875rem;
}

.variable-history-modal {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0, 0, 0, 0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 2000;
}

.variable-history-modal .modal-content {
    background: white;
    padding: 2rem;
    border-radius: 8px;
    max-width: 800px;
    max-height: 80vh;
    overflow-y: auto;
}

.history-chart {
    height: 300px;
    margin: 1rem 0;
}

.history-table {
    max-height: 300px;
    overflow-y: auto;
}

.history-table table {
    width: 100%;
    border-collapse: collapse;
}

.history-table th,
.history-table td {
    padding: 0.5rem;
    border-bottom: 1px solid #dee2e6;
    text-align: left;
}

.diff-table {
    width: 100%;
    border-collapse: collapse;
    margin: 0.5rem 0;
}

.diff-table th,
.diff-table td {
    padding: 0.25rem 0.5rem;
    border: 1px solid #dee2e6;
    font-size: 0.875rem;
}

.variable-stats {
    display: flex;
    gap: 1rem;
    margin-bottom: 0.5rem;
    padding: 0.5rem;
    background-color: #f8f9fa;
    border-radius: 4px;
}

.stat-item {
    display: flex;
    gap: 0.5rem;
}

.stat-label {
    font-weight: bold;
    color: #6c757d;
}

.stat-value {
    color: #212529;
}
`;
document.head.appendChild(style);