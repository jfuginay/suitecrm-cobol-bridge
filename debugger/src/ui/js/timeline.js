class TimelineVisualization {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.timeline = null;
        this.currentStep = 0;
        this.scale = 1;
        this.offset = 0;
        this.hoveredStep = null;
        this.selectedSteps = new Set();
        
        this.setupCanvas();
        this.attachEventListeners();
    }

    setupCanvas() {
        // Set canvas size
        const container = this.canvas.parentElement;
        this.canvas.width = container.clientWidth - 20;
        this.canvas.height = 100;
        
        // High DPI support
        const dpr = window.devicePixelRatio || 1;
        this.canvas.width *= dpr;
        this.canvas.height *= dpr;
        this.ctx.scale(dpr, dpr);
        this.canvas.style.width = `${container.clientWidth - 20}px`;
        this.canvas.style.height = '100px';
    }

    attachEventListeners() {
        this.canvas.addEventListener('mousemove', this.handleMouseMove.bind(this));
        this.canvas.addEventListener('click', this.handleClick.bind(this));
        this.canvas.addEventListener('wheel', this.handleWheel.bind(this));
        
        // Touch support
        let touchStart = 0;
        this.canvas.addEventListener('touchstart', (e) => {
            touchStart = e.touches[0].clientX;
        });
        
        this.canvas.addEventListener('touchmove', (e) => {
            const touchCurrent = e.touches[0].clientX;
            const delta = touchCurrent - touchStart;
            this.offset += delta;
            touchStart = touchCurrent;
            this.render();
        });
    }

    loadTimeline(timeline) {
        this.timeline = timeline;
        this.currentStep = 0;
        this.render();
    }

    setCurrentStep(step) {
        this.currentStep = step;
        this.render();
    }

    render() {
        if (!this.timeline) return;
        
        const width = this.canvas.width / (window.devicePixelRatio || 1);
        const height = this.canvas.height / (window.devicePixelRatio || 1);
        
        // Clear canvas
        this.ctx.clearRect(0, 0, width, height);
        
        // Calculate step width
        const stepCount = this.timeline.steps.length;
        const stepWidth = Math.max(2, (width * this.scale) / stepCount);
        
        // Draw background
        this.ctx.fillStyle = '#f5f5f5';
        this.ctx.fillRect(0, 0, width, height);
        
        // Draw grid lines
        this.drawGrid(width, height);
        
        // Draw steps
        for (let i = 0; i < stepCount; i++) {
            const step = this.timeline.steps[i];
            const x = this.offset + (i * stepWidth);
            
            if (x + stepWidth < 0 || x > width) continue; // Skip off-screen steps
            
            this.drawStep(step, x, stepWidth, height);
        }
        
        // Draw current step indicator
        this.drawCurrentStepIndicator(width, height, stepWidth);
        
        // Draw hover tooltip
        if (this.hoveredStep !== null) {
            this.drawTooltip(this.hoveredStep, width, height, stepWidth);
        }
    }

    drawGrid(width, height) {
        this.ctx.strokeStyle = '#e0e0e0';
        this.ctx.lineWidth = 0.5;
        
        // Vertical lines every 100ms
        const msPerPixel = this.timeline.duration / (width * this.scale);
        const gridInterval = 100; // 100ms
        
        for (let ms = 0; ms <= this.timeline.duration; ms += gridInterval) {
            const x = this.offset + (ms / msPerPixel);
            if (x < 0 || x > width) continue;
            
            this.ctx.beginPath();
            this.ctx.moveTo(x, 0);
            this.ctx.lineTo(x, height);
            this.ctx.stroke();
            
            // Time label
            this.ctx.fillStyle = '#666';
            this.ctx.font = '10px monospace';
            this.ctx.fillText(`${ms}ms`, x + 2, height - 5);
        }
    }

    drawStep(step, x, width, height) {
        const barHeight = height - 20;
        let color = '#4CAF50'; // Default green
        
        // Color based on patterns
        if (step.patterns && step.patterns.length > 0) {
            const severity = this.getMaxSeverity(step.patterns);
            switch (severity) {
                case 'CRITICAL':
                    color = '#f44336'; // Red
                    break;
                case 'HIGH':
                    color = '#ff9800'; // Orange
                    break;
                case 'MEDIUM':
                    color = '#ffc107'; // Yellow
                    break;
                default:
                    color = '#2196f3'; // Blue
            }
        }
        
        // Highlight current step
        if (step.index === this.currentStep) {
            this.ctx.fillStyle = color;
            this.ctx.fillRect(x - 2, 0, width + 4, barHeight);
            
            // Add glow effect
            this.ctx.shadowColor = color;
            this.ctx.shadowBlur = 10;
            this.ctx.fillRect(x, 2, width, barHeight - 4);
            this.ctx.shadowBlur = 0;
        } else {
            this.ctx.fillStyle = color;
            this.ctx.fillRect(x, 10, width - 1, barHeight - 20);
        }
        
        // Draw paragraph name if space allows
        if (width > 30) {
            this.ctx.save();
            this.ctx.translate(x + width / 2, barHeight / 2);
            this.ctx.rotate(-Math.PI / 2);
            this.ctx.fillStyle = '#333';
            this.ctx.font = '10px monospace';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(step.paragraph.substring(0, 10), 0, 0);
            this.ctx.restore();
        }
    }

    drawCurrentStepIndicator(width, height, stepWidth) {
        const x = this.offset + (this.currentStep * stepWidth);
        
        // Draw triangle indicator
        this.ctx.fillStyle = '#333';
        this.ctx.beginPath();
        this.ctx.moveTo(x + stepWidth / 2 - 5, height - 10);
        this.ctx.lineTo(x + stepWidth / 2 + 5, height - 10);
        this.ctx.lineTo(x + stepWidth / 2, height - 2);
        this.ctx.closePath();
        this.ctx.fill();
    }

    drawTooltip(stepIndex, width, height, stepWidth) {
        const step = this.timeline.steps[stepIndex];
        const x = this.offset + (stepIndex * stepWidth);
        
        // Prepare tooltip content
        const lines = [
            `Step ${step.index}`,
            `${step.paragraph}`,
            `Time: ${step.timestamp}ms`
        ];
        
        if (step.patterns && step.patterns.length > 0) {
            lines.push('Patterns:');
            step.patterns.forEach(p => lines.push(`  - ${p.type}`));
        }
        
        // Calculate tooltip dimensions
        this.ctx.font = '12px monospace';
        const padding = 8;
        const lineHeight = 16;
        const maxWidth = Math.max(...lines.map(line => this.ctx.measureText(line).width));
        const tooltipWidth = maxWidth + padding * 2;
        const tooltipHeight = lines.length * lineHeight + padding * 2;
        
        // Position tooltip
        let tooltipX = x + stepWidth / 2 - tooltipWidth / 2;
        let tooltipY = 5;
        
        // Keep tooltip on screen
        if (tooltipX < 0) tooltipX = 0;
        if (tooltipX + tooltipWidth > width) tooltipX = width - tooltipWidth;
        
        // Draw tooltip background
        this.ctx.fillStyle = 'rgba(0, 0, 0, 0.9)';
        this.ctx.fillRect(tooltipX, tooltipY, tooltipWidth, tooltipHeight);
        
        // Draw tooltip text
        this.ctx.fillStyle = 'white';
        lines.forEach((line, i) => {
            this.ctx.fillText(line, tooltipX + padding, tooltipY + padding + (i + 1) * lineHeight - 4);
        });
    }

    getMaxSeverity(patterns) {
        const severityOrder = ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW', 'INFO'];
        let maxSeverity = 'INFO';
        
        for (const pattern of patterns) {
            const severity = this.getPatternSeverity(pattern.type);
            if (severityOrder.indexOf(severity) < severityOrder.indexOf(maxSeverity)) {
                maxSeverity = severity;
            }
        }
        
        return maxSeverity;
    }

    getPatternSeverity(patternType) {
        const severityMap = {
            'INFINITE_LOOP': 'CRITICAL',
            'DIVISION_BY_ZERO_RISK': 'HIGH',
            'OVERFLOW_RISK': 'HIGH',
            'FILE_ERROR': 'MEDIUM',
            'DATA_TRUNCATION': 'LOW'
        };
        return severityMap[patternType] || 'INFO';
    }

    handleMouseMove(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = event.clientX - rect.left;
        
        if (!this.timeline) return;
        
        const stepCount = this.timeline.steps.length;
        const stepWidth = Math.max(2, (rect.width * this.scale) / stepCount);
        
        // Find hovered step
        const stepIndex = Math.floor((x - this.offset) / stepWidth);
        
        if (stepIndex >= 0 && stepIndex < stepCount) {
            this.hoveredStep = stepIndex;
        } else {
            this.hoveredStep = null;
        }
        
        this.render();
    }

    handleClick(event) {
        if (this.hoveredStep !== null) {
            this.currentStep = this.hoveredStep;
            this.render();
            
            // Emit step change event
            window.dispatchEvent(new CustomEvent('timelineStepChange', {
                detail: { step: this.currentStep }
            }));
        }
    }

    handleWheel(event) {
        event.preventDefault();
        
        const delta = event.deltaY > 0 ? 0.9 : 1.1;
        this.scale *= delta;
        this.scale = Math.max(0.1, Math.min(10, this.scale));
        
        this.render();
    }

    // Timeline control methods
    play() {
        if (!this.timeline || this.playing) return;
        
        this.playing = true;
        this.playInterval = setInterval(() => {
            if (this.currentStep < this.timeline.steps.length - 1) {
                this.currentStep++;
                this.render();
                
                window.dispatchEvent(new CustomEvent('timelineStepChange', {
                    detail: { step: this.currentStep }
                }));
            } else {
                this.pause();
            }
        }, 50); // 20 FPS
    }

    pause() {
        this.playing = false;
        if (this.playInterval) {
            clearInterval(this.playInterval);
            this.playInterval = null;
        }
    }

    stepForward() {
        if (!this.timeline) return;
        
        if (this.currentStep < this.timeline.steps.length - 1) {
            this.currentStep++;
            this.render();
            
            window.dispatchEvent(new CustomEvent('timelineStepChange', {
                detail: { step: this.currentStep }
            }));
        }
    }

    stepBackward() {
        if (!this.timeline) return;
        
        if (this.currentStep > 0) {
            this.currentStep--;
            this.render();
            
            window.dispatchEvent(new CustomEvent('timelineStepChange', {
                detail: { step: this.currentStep }
            }));
        }
    }

    goToStep(stepIndex) {
        if (!this.timeline) return;
        
        if (stepIndex >= 0 && stepIndex < this.timeline.steps.length) {
            this.currentStep = stepIndex;
            this.render();
            
            window.dispatchEvent(new CustomEvent('timelineStepChange', {
                detail: { step: this.currentStep }
            }));
        }
    }

    rewind() {
        this.currentStep = 0;
        this.render();
        
        window.dispatchEvent(new CustomEvent('timelineStepChange', {
            detail: { step: this.currentStep }
        }));
    }

    fastForward() {
        if (!this.timeline) return;
        
        this.currentStep = this.timeline.steps.length - 1;
        this.render();
        
        window.dispatchEvent(new CustomEvent('timelineStepChange', {
            detail: { step: this.currentStep }
        }));
    }

    // Export timeline as image
    exportAsImage() {
        const link = document.createElement('a');
        link.download = `timeline-${this.timeline.traceId}.png`;
        link.href = this.canvas.toDataURL();
        link.click();
    }

    // Get execution statistics for visualization
    getExecutionStats() {
        if (!this.timeline) return null;
        
        const paragraphCounts = {};
        const patternCounts = {};
        const timeDistribution = [];
        
        for (const step of this.timeline.steps) {
            // Count paragraph executions
            paragraphCounts[step.paragraph] = (paragraphCounts[step.paragraph] || 0) + 1;
            
            // Count patterns
            for (const pattern of step.patterns || []) {
                patternCounts[pattern.type] = (patternCounts[pattern.type] || 0) + 1;
            }
            
            // Time distribution
            const bucket = Math.floor(step.timestamp / 100) * 100;
            const existing = timeDistribution.find(t => t.time === bucket);
            if (existing) {
                existing.count++;
            } else {
                timeDistribution.push({ time: bucket, count: 1 });
            }
        }
        
        return {
            paragraphCounts,
            patternCounts,
            timeDistribution,
            totalSteps: this.timeline.steps.length,
            duration: this.timeline.duration
        };
    }
}

// Export for use in other modules
window.TimelineVisualization = TimelineVisualization;