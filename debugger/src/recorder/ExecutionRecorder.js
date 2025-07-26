const { v4: uuidv4 } = require('uuid');
const Timeline = require('../models/Timeline');

class ExecutionRecorder {
    constructor(storage) {
        this.storage = storage;
        this.activeRecordings = new Map();
    }

    async startRecording(programName, metadata = {}) {
        const traceId = uuidv4();
        const timeline = new Timeline(traceId, programName, metadata);
        
        this.activeRecordings.set(traceId, {
            timeline,
            startTime: Date.now(),
            stepCount: 0
        });

        return traceId;
    }

    async recordStep(traceId, stepData) {
        const recording = this.activeRecordings.get(traceId);
        if (!recording) {
            throw new Error(`No active recording for trace ${traceId}`);
        }

        const step = {
            index: recording.stepCount++,
            timestamp: Date.now() - recording.startTime,
            paragraph: stepData.paragraph,
            section: stepData.section,
            line: stepData.line,
            operation: stepData.operation,
            variables: this.captureVariables(stepData.variables),
            memory: this.captureMemoryState(stepData.memory),
            fileHandles: stepData.fileHandles || {},
            performedOperations: stepData.performedOperations || [],
            stackTrace: stepData.stackTrace || [],
            conditions: stepData.conditions || {},
            loopIterations: stepData.loopIterations || 0
        };

        // Detect interesting patterns
        step.patterns = this.detectPatterns(step, recording.timeline);

        recording.timeline.addStep(step);
        return step;
    }

    captureVariables(variables) {
        const captured = {};
        
        for (const [name, value] of Object.entries(variables)) {
            captured[name] = {
                value: value,
                type: this.detectCobolType(value),
                size: this.getVariableSize(value),
                packed: this.isPacked(value),
                numeric: this.isNumeric(value),
                picture: this.inferPictureClause(value)
            };
        }

        return captured;
    }

    captureMemoryState(memory) {
        if (!memory) return {};

        return {
            workingStorage: memory.workingStorage || {},
            localStorage: memory.localStorage || {},
            linkageSection: memory.linkageSection || {},
            fileSection: memory.fileSection || {},
            totalUsed: this.calculateMemoryUsage(memory),
            snapshot: this.createMemorySnapshot(memory)
        };
    }

    detectPatterns(step, timeline) {
        const patterns = [];

        // Detect infinite loops
        if (this.isInfiniteLoop(step, timeline)) {
            patterns.push({
                type: 'INFINITE_LOOP',
                paragraph: step.paragraph,
                iterations: this.countLoopIterations(step, timeline)
            });
        }

        // Detect division by zero risk
        if (this.hasDivisionByZeroRisk(step)) {
            patterns.push({
                type: 'DIVISION_BY_ZERO_RISK',
                variable: this.findZeroVariable(step)
            });
        }

        // Detect overflow conditions
        if (this.hasOverflowRisk(step)) {
            patterns.push({
                type: 'OVERFLOW_RISK',
                variables: this.findOverflowVariables(step)
            });
        }

        // Detect file operation errors
        if (this.hasFileOperationError(step)) {
            patterns.push({
                type: 'FILE_ERROR',
                operation: step.operation,
                fileStatus: step.fileHandles
            });
        }

        // Detect data truncation
        if (this.hasDataTruncation(step)) {
            patterns.push({
                type: 'DATA_TRUNCATION',
                variables: this.findTruncatedVariables(step)
            });
        }

        return patterns;
    }

    isInfiniteLoop(step, timeline) {
        const recentSteps = timeline.getRecentSteps(100);
        const paragraphCounts = {};
        
        for (const s of recentSteps) {
            paragraphCounts[s.paragraph] = (paragraphCounts[s.paragraph] || 0) + 1;
        }

        return paragraphCounts[step.paragraph] > 50;
    }

    countLoopIterations(step, timeline) {
        return timeline.getSteps()
            .filter(s => s.paragraph === step.paragraph)
            .length;
    }

    hasDivisionByZeroRisk(step) {
        if (step.operation !== 'DIVIDE') return false;
        
        for (const [name, info] of Object.entries(step.variables)) {
            if (info.value === 0 || info.value === '0') {
                return true;
            }
        }
        return false;
    }

    findZeroVariable(step) {
        for (const [name, info] of Object.entries(step.variables)) {
            if (info.value === 0 || info.value === '0') {
                return name;
            }
        }
        return null;
    }

    hasOverflowRisk(step) {
        for (const [name, info] of Object.entries(step.variables)) {
            if (info.numeric && this.isNearLimit(info)) {
                return true;
            }
        }
        return false;
    }

    isNearLimit(varInfo) {
        if (!varInfo.picture) return false;
        
        const maxValue = this.getMaxValueForPicture(varInfo.picture);
        const currentValue = parseFloat(varInfo.value);
        
        return currentValue > maxValue * 0.9;
    }

    getMaxValueForPicture(picture) {
        // Parse COBOL picture clause to determine max value
        const match = picture.match(/9\((\d+)\)/);
        if (match) {
            const digits = parseInt(match[1]);
            return Math.pow(10, digits) - 1;
        }
        return 999999999; // Default large value
    }

    findOverflowVariables(step) {
        const vars = [];
        for (const [name, info] of Object.entries(step.variables)) {
            if (info.numeric && this.isNearLimit(info)) {
                vars.push(name);
            }
        }
        return vars;
    }

    hasFileOperationError(step) {
        return step.fileHandles && 
               Object.values(step.fileHandles).some(status => 
                   status && status !== '00' && status !== '10'
               );
    }

    hasDataTruncation(step) {
        for (const op of step.performedOperations) {
            if (op.type === 'MOVE' && op.truncated) {
                return true;
            }
        }
        return false;
    }

    findTruncatedVariables(step) {
        const vars = [];
        for (const op of step.performedOperations) {
            if (op.type === 'MOVE' && op.truncated) {
                vars.push(op.target);
            }
        }
        return vars;
    }

    detectCobolType(value) {
        if (typeof value === 'number') return 'NUMERIC';
        if (typeof value === 'string') {
            if (/^\d+$/.test(value)) return 'NUMERIC-STRING';
            if (/^\d+\.\d+$/.test(value)) return 'DECIMAL';
            return 'ALPHANUMERIC';
        }
        return 'UNKNOWN';
    }

    getVariableSize(value) {
        if (typeof value === 'string') return value.length;
        if (typeof value === 'number') return value.toString().length;
        return 0;
    }

    isPacked(value) {
        // Check if value appears to be packed decimal
        return typeof value === 'object' && value.packed === true;
    }

    isNumeric(value) {
        return !isNaN(value) && !isNaN(parseFloat(value));
    }

    inferPictureClause(value) {
        if (typeof value === 'number') {
            const str = value.toString();
            if (str.includes('.')) {
                const parts = str.split('.');
                return `9(${parts[0].length})V9(${parts[1].length})`;
            }
            return `9(${str.length})`;
        }
        if (typeof value === 'string') {
            if (/^\d+$/.test(value)) return `9(${value.length})`;
            return `X(${value.length})`;
        }
        return 'UNKNOWN';
    }

    calculateMemoryUsage(memory) {
        let total = 0;
        for (const section of Object.values(memory)) {
            if (typeof section === 'object') {
                total += JSON.stringify(section).length;
            }
        }
        return total;
    }

    createMemorySnapshot(memory) {
        return {
            timestamp: Date.now(),
            sections: Object.keys(memory),
            checksum: this.calculateChecksum(memory)
        };
    }

    calculateChecksum(obj) {
        const str = JSON.stringify(obj);
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash;
        }
        return hash.toString(16);
    }

    async finishRecording(traceId) {
        const recording = this.activeRecordings.get(traceId);
        if (!recording) {
            throw new Error(`No active recording for trace ${traceId}`);
        }

        recording.timeline.finalize({
            duration: Date.now() - recording.startTime,
            stepCount: recording.stepCount
        });

        await this.storage.saveTrace(recording.timeline);
        this.activeRecordings.delete(traceId);

        return recording.timeline;
    }

    async replay(timeline, modifiedInputs = {}) {
        const replayId = uuidv4();
        const replayTimeline = timeline.clone();
        
        // Apply modified inputs
        replayTimeline.applyInputModifications(modifiedInputs);

        // Simulate replay execution
        const results = [];
        for (const step of replayTimeline.getSteps()) {
            const replayStep = await this.simulateStep(step, modifiedInputs);
            results.push(replayStep);
        }

        return {
            replayId,
            original: timeline.getTraceId(),
            modifiedInputs,
            results,
            differences: this.compareExecutions(timeline, results)
        };
    }

    async simulateStep(step, modifiedInputs) {
        // Simulate execution with modified inputs
        const simulatedStep = { ...step };
        
        // Apply input modifications
        if (modifiedInputs[step.paragraph]) {
            simulatedStep.variables = {
                ...step.variables,
                ...modifiedInputs[step.paragraph]
            };
        }

        // Re-evaluate conditions and operations
        simulatedStep.simulationResult = this.evaluateStep(simulatedStep);
        
        return simulatedStep;
    }

    evaluateStep(step) {
        // Simulate COBOL operations
        const result = {
            success: true,
            changes: {},
            warnings: []
        };

        // Check for potential issues with new values
        if (this.hasDivisionByZeroRisk(step)) {
            result.success = false;
            result.error = 'Division by zero';
        }

        if (this.hasOverflowRisk(step)) {
            result.warnings.push('Potential overflow');
        }

        return result;
    }

    compareExecutions(original, replayed) {
        const differences = [];
        const originalSteps = original.getSteps();

        for (let i = 0; i < originalSteps.length && i < replayed.length; i++) {
            const diff = this.compareSteps(originalSteps[i], replayed[i]);
            if (diff.hasDifferences) {
                differences.push({
                    stepIndex: i,
                    differences: diff
                });
            }
        }

        return differences;
    }

    compareSteps(original, replayed) {
        const diff = {
            hasDifferences: false,
            variables: {},
            conditions: {},
            operations: []
        };

        // Compare variables
        for (const varName of Object.keys(original.variables)) {
            if (original.variables[varName].value !== replayed.variables[varName]?.value) {
                diff.hasDifferences = true;
                diff.variables[varName] = {
                    original: original.variables[varName].value,
                    replayed: replayed.variables[varName]?.value
                };
            }
        }

        return diff;
    }
}

module.exports = ExecutionRecorder;