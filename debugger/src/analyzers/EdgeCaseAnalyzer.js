class EdgeCaseAnalyzer {
    constructor() {
        this.severityLevels = {
            CRITICAL: 4,
            HIGH: 3,
            MEDIUM: 2,
            LOW: 1,
            INFO: 0
        };

        this.patternAnalyzers = {
            'INFINITE_LOOP': this.analyzeInfiniteLoop.bind(this),
            'DIVISION_BY_ZERO_RISK': this.analyzeDivisionByZero.bind(this),
            'OVERFLOW_RISK': this.analyzeOverflow.bind(this),
            'FILE_ERROR': this.analyzeFileError.bind(this),
            'DATA_TRUNCATION': this.analyzeDataTruncation.bind(this),
            'UNINITIALIZED_VARIABLE': this.analyzeUninitialized.bind(this),
            'BOUNDARY_VIOLATION': this.analyzeBoundaryViolation.bind(this),
            'RACE_CONDITION': this.analyzeRaceCondition.bind(this)
        };
    }

    analyze(timeline) {
        const edgeCases = [];
        const patterns = new Map();
        const statistics = {
            totalSteps: timeline.steps.length,
            paragraphExecutions: {},
            variableChanges: {},
            fileOperations: 0,
            arithmeticOperations: 0,
            conditionalBranches: 0
        };

        // First pass: collect statistics and patterns
        timeline.steps.forEach(step => {
            // Track paragraph executions
            statistics.paragraphExecutions[step.paragraph] = 
                (statistics.paragraphExecutions[step.paragraph] || 0) + 1;

            // Track variable changes
            Object.keys(step.variables).forEach(varName => {
                statistics.variableChanges[varName] = 
                    (statistics.variableChanges[varName] || 0) + 1;
            });

            // Count operations
            if (step.operation) {
                if (step.operation.includes('READ') || step.operation.includes('WRITE')) {
                    statistics.fileOperations++;
                }
                if (step.operation.includes('COMPUTE') || step.operation.includes('DIVIDE')) {
                    statistics.arithmeticOperations++;
                }
            }

            // Collect patterns
            if (step.patterns && step.patterns.length > 0) {
                step.patterns.forEach(pattern => {
                    if (!patterns.has(pattern.type)) {
                        patterns.set(pattern.type, []);
                    }
                    patterns.get(pattern.type).push({
                        stepIndex: step.index,
                        details: pattern,
                        context: step
                    });
                });
            }
        });

        // Second pass: deep analysis
        this.analyzeExecutionPatterns(timeline, edgeCases, statistics);
        this.analyzeVariablePatterns(timeline, edgeCases, statistics);
        this.analyzeControlFlow(timeline, edgeCases);
        this.analyzeDataFlow(timeline, edgeCases);

        // Analyze collected patterns
        patterns.forEach((instances, patternType) => {
            const analyzer = this.patternAnalyzers[patternType];
            if (analyzer) {
                const analysis = analyzer(instances, timeline);
                if (analysis) {
                    edgeCases.push(analysis);
                }
            }
        });

        // Sort by severity
        edgeCases.sort((a, b) => 
            this.severityLevels[b.severity] - this.severityLevels[a.severity]
        );

        return {
            edgeCases,
            statistics,
            summary: this.generateSummary(edgeCases, statistics),
            recommendations: this.generateRecommendations(edgeCases)
        };
    }

    analyzeExecutionPatterns(timeline, edgeCases, statistics) {
        // Check for hot spots
        Object.entries(statistics.paragraphExecutions).forEach(([paragraph, count]) => {
            if (count > timeline.steps.length * 0.3) {
                edgeCases.push({
                    type: 'HOT_SPOT',
                    severity: 'MEDIUM',
                    paragraph,
                    count,
                    percentage: (count / timeline.steps.length * 100).toFixed(2),
                    description: `Paragraph ${paragraph} executed ${count} times (${(count / timeline.steps.length * 100).toFixed(2)}% of all steps)`,
                    recommendation: 'Consider optimizing this frequently executed paragraph'
                });
            }
        });

        // Check for potential infinite loops
        const sequentialExecutions = this.findSequentialExecutions(timeline);
        sequentialExecutions.forEach(seq => {
            if (seq.count > 100) {
                edgeCases.push({
                    type: 'POTENTIAL_INFINITE_LOOP',
                    severity: 'CRITICAL',
                    paragraph: seq.paragraph,
                    startStep: seq.startStep,
                    count: seq.count,
                    description: `Paragraph ${seq.paragraph} executed ${seq.count} times consecutively`,
                    recommendation: 'Add loop termination condition or counter'
                });
            }
        });
    }

    analyzeVariablePatterns(timeline, edgeCases, statistics) {
        // Check for variables that never change
        const firstStep = timeline.steps[0];
        const lastStep = timeline.steps[timeline.steps.length - 1];

        if (firstStep && lastStep) {
            Object.entries(firstStep.variables).forEach(([varName, firstValue]) => {
                const lastValue = lastStep.variables[varName];
                if (lastValue && firstValue.value === lastValue.value && 
                    statistics.variableChanges[varName] === 1) {
                    edgeCases.push({
                        type: 'CONSTANT_VARIABLE',
                        severity: 'INFO',
                        variable: varName,
                        value: firstValue.value,
                        description: `Variable ${varName} never changes from initial value`,
                        recommendation: 'Consider using a constant instead'
                    });
                }
            });
        }

        // Check for rapid variable changes
        Object.entries(statistics.variableChanges).forEach(([varName, changeCount]) => {
            const changeRate = changeCount / timeline.steps.length;
            if (changeRate > 0.5) {
                edgeCases.push({
                    type: 'HIGH_CHANGE_RATE',
                    severity: 'LOW',
                    variable: varName,
                    changeCount,
                    changeRate: (changeRate * 100).toFixed(2),
                    description: `Variable ${varName} changes in ${(changeRate * 100).toFixed(2)}% of steps`,
                    recommendation: 'Review if all changes are necessary'
                });
            }
        });
    }

    analyzeControlFlow(timeline, edgeCases) {
        const callGraph = timeline.getCallGraph();
        
        // Check for unreachable paragraphs
        const executedParagraphs = new Set(callGraph.nodes);
        
        // Check for circular dependencies
        const cycles = this.findCycles(callGraph);
        cycles.forEach(cycle => {
            edgeCases.push({
                type: 'CIRCULAR_DEPENDENCY',
                severity: 'HIGH',
                cycle: cycle.join(' -> '),
                description: `Circular dependency detected: ${cycle.join(' -> ')}`,
                recommendation: 'Refactor to eliminate circular calls'
            });
        });

        // Check for deep call stacks
        const maxDepth = this.findMaxCallDepth(timeline);
        if (maxDepth > 10) {
            edgeCases.push({
                type: 'DEEP_CALL_STACK',
                severity: 'MEDIUM',
                depth: maxDepth,
                description: `Maximum call depth of ${maxDepth} detected`,
                recommendation: 'Consider flattening the call hierarchy'
            });
        }
    }

    analyzeDataFlow(timeline, edgeCases) {
        // Track data dependencies
        const dataDependencies = new Map();
        
        timeline.steps.forEach((step, index) => {
            // Check for uninitialized variables
            Object.entries(step.variables).forEach(([varName, varInfo]) => {
                if (varInfo.value === null || varInfo.value === undefined) {
                    edgeCases.push({
                        type: 'UNINITIALIZED_VARIABLE',
                        severity: 'HIGH',
                        variable: varName,
                        stepIndex: index,
                        paragraph: step.paragraph,
                        description: `Variable ${varName} used without initialization in ${step.paragraph}`,
                        recommendation: 'Initialize variable before use'
                    });
                }
            });

            // Track data flow
            if (step.operation === 'MOVE') {
                // Track variable dependencies
                step.performedOperations.forEach(op => {
                    if (op.source && op.target) {
                        if (!dataDependencies.has(op.target)) {
                            dataDependencies.set(op.target, new Set());
                        }
                        dataDependencies.get(op.target).add(op.source);
                    }
                });
            }
        });

        // Check for complex dependencies
        dataDependencies.forEach((sources, target) => {
            if (sources.size > 5) {
                edgeCases.push({
                    type: 'COMPLEX_DEPENDENCY',
                    severity: 'LOW',
                    variable: target,
                    sourceCount: sources.size,
                    sources: Array.from(sources).slice(0, 5),
                    description: `Variable ${target} depends on ${sources.size} different sources`,
                    recommendation: 'Consider simplifying data dependencies'
                });
            }
        });
    }

    analyzeInfiniteLoop(instances, timeline) {
        if (instances.length === 0) return null;

        const mostSevere = instances.reduce((max, curr) => 
            curr.details.iterations > (max.details.iterations || 0) ? curr : max
        );

        return {
            type: 'INFINITE_LOOP',
            severity: 'CRITICAL',
            instances: instances.length,
            worstCase: {
                paragraph: mostSevere.context.paragraph,
                iterations: mostSevere.details.iterations,
                stepIndex: mostSevere.stepIndex
            },
            description: `Infinite loop detected in ${instances.length} location(s)`,
            recommendation: 'Add loop counters and termination conditions',
            recommendedPatch: 'loop-termination'
        };
    }

    analyzeDivisionByZero(instances, timeline) {
        if (instances.length === 0) return null;

        return {
            type: 'DIVISION_BY_ZERO_RISK',
            severity: 'HIGH',
            instances: instances.length,
            locations: instances.map(i => ({
                paragraph: i.context.paragraph,
                stepIndex: i.stepIndex,
                variable: i.details.variable
            })),
            description: `Division by zero risk in ${instances.length} location(s)`,
            recommendation: 'Add divisor validation before division operations',
            recommendedPatch: 'division-guard'
        };
    }

    analyzeOverflow(instances, timeline) {
        if (instances.length === 0) return null;

        return {
            type: 'OVERFLOW_RISK',
            severity: 'HIGH',
            instances: instances.length,
            variables: [...new Set(instances.flatMap(i => i.details.variables))],
            description: `Numeric overflow risk for ${instances.length} variable(s)`,
            recommendation: 'Add boundary checks and ON SIZE ERROR handling',
            recommendedPatch: 'overflow-protection'
        };
    }

    analyzeFileError(instances, timeline) {
        if (instances.length === 0) return null;

        const errorCodes = {};
        instances.forEach(i => {
            Object.entries(i.context.fileHandles).forEach(([file, status]) => {
                if (status !== '00' && status !== '10') {
                    errorCodes[status] = (errorCodes[status] || 0) + 1;
                }
            });
        });

        return {
            type: 'FILE_ERROR',
            severity: 'MEDIUM',
            instances: instances.length,
            errorCodes,
            description: `File operation errors in ${instances.length} location(s)`,
            recommendation: 'Implement comprehensive file error handling',
            recommendedPatch: 'file-error-handling'
        };
    }

    analyzeDataTruncation(instances, timeline) {
        if (instances.length === 0) return null;

        const truncatedVars = [...new Set(instances.flatMap(i => i.details.variables))];

        return {
            type: 'DATA_TRUNCATION',
            severity: 'LOW',
            instances: instances.length,
            variables: truncatedVars,
            description: `Data truncation detected for ${truncatedVars.length} variable(s)`,
            recommendation: 'Review PICTURE clauses and ensure adequate field sizes',
            recommendedPatch: 'boundary-check'
        };
    }

    analyzeUninitialized(instances, timeline) {
        return {
            type: 'UNINITIALIZED_VARIABLE',
            severity: 'HIGH',
            instances: instances.length,
            description: 'Variables used without proper initialization',
            recommendation: 'Initialize all variables in WORKING-STORAGE',
            recommendedPatch: 'null-check'
        };
    }

    analyzeBoundaryViolation(instances, timeline) {
        return {
            type: 'BOUNDARY_VIOLATION',
            severity: 'HIGH',
            instances: instances.length,
            description: 'Values exceeding defined PICTURE clause limits',
            recommendation: 'Add boundary validation',
            recommendedPatch: 'boundary-check'
        };
    }

    analyzeRaceCondition(instances, timeline) {
        return {
            type: 'RACE_CONDITION',
            severity: 'CRITICAL',
            instances: instances.length,
            description: 'Potential race condition in concurrent access',
            recommendation: 'Implement proper locking mechanisms'
        };
    }

    findSequentialExecutions(timeline) {
        const sequences = [];
        let currentSeq = null;

        timeline.steps.forEach((step, index) => {
            if (currentSeq && currentSeq.paragraph === step.paragraph) {
                currentSeq.count++;
            } else {
                if (currentSeq && currentSeq.count > 10) {
                    sequences.push(currentSeq);
                }
                currentSeq = {
                    paragraph: step.paragraph,
                    startStep: index,
                    count: 1
                };
            }
        });

        if (currentSeq && currentSeq.count > 10) {
            sequences.push(currentSeq);
        }

        return sequences;
    }

    findCycles(callGraph) {
        const cycles = [];
        const visited = new Set();
        const recursionStack = new Set();

        const dfs = (node, path) => {
            visited.add(node);
            recursionStack.add(node);

            const edges = callGraph.edges.filter(e => e.from === node);
            for (const edge of edges) {
                if (!visited.has(edge.to)) {
                    dfs(edge.to, [...path, edge.to]);
                } else if (recursionStack.has(edge.to)) {
                    // Found a cycle
                    const cycleStart = path.indexOf(edge.to);
                    cycles.push(path.slice(cycleStart).concat(edge.to));
                }
            }

            recursionStack.delete(node);
        };

        callGraph.nodes.forEach(node => {
            if (!visited.has(node)) {
                dfs(node, [node]);
            }
        });

        return cycles;
    }

    findMaxCallDepth(timeline) {
        let maxDepth = 0;
        let currentDepth = 0;
        const callStack = [];

        timeline.steps.forEach(step => {
            // Simple heuristic: track PERFORM depth
            if (step.operation && step.operation.includes('PERFORM')) {
                callStack.push(step.paragraph);
                currentDepth++;
                maxDepth = Math.max(maxDepth, currentDepth);
            } else if (step.operation && step.operation.includes('EXIT')) {
                if (callStack.length > 0) {
                    callStack.pop();
                    currentDepth--;
                }
            }
        });

        return maxDepth;
    }

    generateSummary(edgeCases, statistics) {
        const summary = {
            totalIssues: edgeCases.length,
            bySeverity: {},
            byType: {},
            criticalIssues: [],
            statistics
        };

        // Count by severity
        Object.keys(this.severityLevels).forEach(severity => {
            summary.bySeverity[severity] = 0;
        });

        // Analyze edge cases
        edgeCases.forEach(ec => {
            summary.bySeverity[ec.severity] = (summary.bySeverity[ec.severity] || 0) + 1;
            summary.byType[ec.type] = (summary.byType[ec.type] || 0) + 1;
            
            if (ec.severity === 'CRITICAL') {
                summary.criticalIssues.push({
                    type: ec.type,
                    description: ec.description
                });
            }
        });

        return summary;
    }

    generateRecommendations(edgeCases) {
        const recommendations = [];
        const typeCount = {};

        // Count issue types
        edgeCases.forEach(ec => {
            typeCount[ec.type] = (typeCount[ec.type] || 0) + 1;
        });

        // Generate priority recommendations
        if (typeCount['INFINITE_LOOP'] > 0) {
            recommendations.push({
                priority: 1,
                action: 'Add loop counters and termination conditions',
                impact: 'Prevents program hangs and resource exhaustion'
            });
        }

        if (typeCount['DIVISION_BY_ZERO_RISK'] > 0) {
            recommendations.push({
                priority: 2,
                action: 'Validate divisors before division operations',
                impact: 'Prevents runtime abends'
            });
        }

        if (typeCount['OVERFLOW_RISK'] > 0) {
            recommendations.push({
                priority: 3,
                action: 'Add ON SIZE ERROR clauses to arithmetic operations',
                impact: 'Handles numeric overflow gracefully'
            });
        }

        if (typeCount['FILE_ERROR'] > 0) {
            recommendations.push({
                priority: 4,
                action: 'Implement comprehensive file status checking',
                impact: 'Improves error recovery and data integrity'
            });
        }

        if (typeCount['UNINITIALIZED_VARIABLE'] > 0) {
            recommendations.push({
                priority: 5,
                action: 'Initialize all variables with VALUE clauses',
                impact: 'Prevents unpredictable behavior'
            });
        }

        return recommendations.sort((a, b) => a.priority - b.priority);
    }

    compareExecutions(timeline1, timeline2) {
        const comparison = {
            timeline1: timeline1.getTraceId(),
            timeline2: timeline2.getTraceId(),
            differences: [],
            similarities: [],
            divergencePoint: null
        };

        const steps1 = timeline1.getSteps();
        const steps2 = timeline2.getSteps();
        const minLength = Math.min(steps1.length, steps2.length);

        // Find divergence point
        for (let i = 0; i < minLength; i++) {
            const step1 = steps1[i];
            const step2 = steps2[i];

            if (step1.paragraph !== step2.paragraph) {
                comparison.divergencePoint = {
                    stepIndex: i,
                    timeline1Paragraph: step1.paragraph,
                    timeline2Paragraph: step2.paragraph
                };
                break;
            }

            // Compare variables
            const varDiff = this.compareVariables(step1.variables, step2.variables);
            if (varDiff.hasDifferences) {
                comparison.differences.push({
                    stepIndex: i,
                    type: 'VARIABLE_DIFFERENCE',
                    differences: varDiff
                });
            }
        }

        // Analyze patterns in both timelines
        const patterns1 = new Set(steps1.flatMap(s => s.patterns?.map(p => p.type) || []));
        const patterns2 = new Set(steps2.flatMap(s => s.patterns?.map(p => p.type) || []));

        const commonPatterns = [...patterns1].filter(p => patterns2.has(p));
        const uniqueToTimeline1 = [...patterns1].filter(p => !patterns2.has(p));
        const uniqueToTimeline2 = [...patterns2].filter(p => !patterns1.has(p));

        comparison.patternAnalysis = {
            common: commonPatterns,
            uniqueToTimeline1,
            uniqueToTimeline2
        };

        return comparison;
    }

    compareVariables(vars1, vars2) {
        const diff = {
            hasDifferences: false,
            added: [],
            removed: [],
            changed: []
        };

        // Check for changes and additions
        Object.entries(vars2).forEach(([name, info2]) => {
            const info1 = vars1[name];
            if (!info1) {
                diff.added.push(name);
                diff.hasDifferences = true;
            } else if (info1.value !== info2.value) {
                diff.changed.push({
                    name,
                    oldValue: info1.value,
                    newValue: info2.value
                });
                diff.hasDifferences = true;
            }
        });

        // Check for removals
        Object.keys(vars1).forEach(name => {
            if (!vars2[name]) {
                diff.removed.push(name);
                diff.hasDifferences = true;
            }
        });

        return diff;
    }
}

module.exports = EdgeCaseAnalyzer;