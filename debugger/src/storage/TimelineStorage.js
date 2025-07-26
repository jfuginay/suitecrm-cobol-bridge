const sqlite3 = require('sqlite3').verbose();
const path = require('path');
const fs = require('fs').promises;
const LZString = require('lz-string');
const Timeline = require('../models/Timeline');

class TimelineStorage {
    constructor(dbPath = path.join(__dirname, '../../data/timelines.db')) {
        this.dbPath = dbPath;
        this.db = null;
        this.init();
    }

    async init() {
        // Ensure data directory exists
        const dataDir = path.dirname(this.dbPath);
        await fs.mkdir(dataDir, { recursive: true });

        // Initialize database
        this.db = new sqlite3.Database(this.dbPath);
        
        await this.createTables();
    }

    createTables() {
        return new Promise((resolve, reject) => {
            this.db.serialize(() => {
                // Main traces table
                this.db.run(`
                    CREATE TABLE IF NOT EXISTS traces (
                        trace_id TEXT PRIMARY KEY,
                        program_name TEXT NOT NULL,
                        created_at INTEGER NOT NULL,
                        duration INTEGER,
                        step_count INTEGER,
                        metadata TEXT,
                        compressed_data TEXT,
                        summary TEXT,
                        tags TEXT
                    )
                `);

                // Steps table for quick queries
                this.db.run(`
                    CREATE TABLE IF NOT EXISTS steps (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        trace_id TEXT NOT NULL,
                        step_index INTEGER NOT NULL,
                        timestamp INTEGER NOT NULL,
                        paragraph TEXT NOT NULL,
                        section TEXT,
                        variables TEXT,
                        patterns TEXT,
                        FOREIGN KEY (trace_id) REFERENCES traces(trace_id)
                    )
                `);

                // Patterns table for analysis
                this.db.run(`
                    CREATE TABLE IF NOT EXISTS patterns (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        trace_id TEXT NOT NULL,
                        pattern_type TEXT NOT NULL,
                        step_index INTEGER NOT NULL,
                        details TEXT,
                        severity TEXT,
                        FOREIGN KEY (trace_id) REFERENCES traces(trace_id)
                    )
                `);

                // Create indexes
                this.db.run(`CREATE INDEX IF NOT EXISTS idx_steps_trace ON steps(trace_id)`);
                this.db.run(`CREATE INDEX IF NOT EXISTS idx_steps_paragraph ON steps(paragraph)`);
                this.db.run(`CREATE INDEX IF NOT EXISTS idx_patterns_trace ON patterns(trace_id)`);
                this.db.run(`CREATE INDEX IF NOT EXISTS idx_patterns_type ON patterns(pattern_type)`);
                
                resolve();
            });
        });
    }

    async saveTrace(timeline) {
        const traceData = timeline.toJSON();
        const compressed = LZString.compressToUTF16(JSON.stringify(traceData));
        
        return new Promise((resolve, reject) => {
            this.db.serialize(() => {
                // Start transaction
                this.db.run('BEGIN TRANSACTION');

                try {
                    // Insert main trace record
                    const stmt = this.db.prepare(`
                        INSERT INTO traces (
                            trace_id, program_name, created_at, duration,
                            step_count, metadata, compressed_data, summary, tags
                        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    `);

                    stmt.run(
                        timeline.traceId,
                        timeline.programName,
                        timeline.createdAt,
                        timeline.duration,
                        timeline.steps.length,
                        JSON.stringify(timeline.metadata),
                        compressed,
                        JSON.stringify(timeline.getSummary()),
                        JSON.stringify(timeline.tags || [])
                    );
                    stmt.finalize();

                    // Insert steps for indexing
                    const stepStmt = this.db.prepare(`
                        INSERT INTO steps (
                            trace_id, step_index, timestamp, paragraph,
                            section, variables, patterns
                        ) VALUES (?, ?, ?, ?, ?, ?, ?)
                    `);

                    for (const step of timeline.steps) {
                        stepStmt.run(
                            timeline.traceId,
                            step.index,
                            step.timestamp,
                            step.paragraph,
                            step.section,
                            JSON.stringify(step.variables),
                            JSON.stringify(step.patterns)
                        );
                    }
                    stepStmt.finalize();

                    // Insert patterns for analysis
                    const patternStmt = this.db.prepare(`
                        INSERT INTO patterns (
                            trace_id, pattern_type, step_index, details, severity
                        ) VALUES (?, ?, ?, ?, ?)
                    `);

                    for (const step of timeline.steps) {
                        for (const pattern of step.patterns || []) {
                            patternStmt.run(
                                timeline.traceId,
                                pattern.type,
                                step.index,
                                JSON.stringify(pattern),
                                this.calculateSeverity(pattern)
                            );
                        }
                    }
                    patternStmt.finalize();

                    // Commit transaction
                    this.db.run('COMMIT', (err) => {
                        if (err) reject(err);
                        else resolve(timeline.traceId);
                    });
                } catch (error) {
                    this.db.run('ROLLBACK');
                    reject(error);
                }
            });
        });
    }

    calculateSeverity(pattern) {
        const severityMap = {
            'INFINITE_LOOP': 'CRITICAL',
            'DIVISION_BY_ZERO_RISK': 'HIGH',
            'OVERFLOW_RISK': 'HIGH',
            'FILE_ERROR': 'MEDIUM',
            'DATA_TRUNCATION': 'LOW'
        };
        return severityMap[pattern.type] || 'INFO';
    }

    async loadTrace(traceId) {
        return new Promise((resolve, reject) => {
            this.db.get(
                'SELECT * FROM traces WHERE trace_id = ?',
                [traceId],
                async (err, row) => {
                    if (err) {
                        reject(err);
                    } else if (!row) {
                        reject(new Error(`Trace ${traceId} not found`));
                    } else {
                        const decompressed = LZString.decompressFromUTF16(row.compressed_data);
                        const traceData = JSON.parse(decompressed);
                        const timeline = Timeline.fromJSON(traceData);
                        resolve(timeline);
                    }
                }
            );
        });
    }

    async listTraces(filters = {}) {
        let query = 'SELECT trace_id, program_name, created_at, duration, step_count, summary FROM traces';
        const params = [];
        const conditions = [];

        if (filters.programName) {
            conditions.push('program_name = ?');
            params.push(filters.programName);
        }

        if (filters.startDate) {
            conditions.push('created_at >= ?');
            params.push(filters.startDate);
        }

        if (filters.endDate) {
            conditions.push('created_at <= ?');
            params.push(filters.endDate);
        }

        if (filters.hasPatterns) {
            conditions.push(`trace_id IN (
                SELECT DISTINCT trace_id FROM patterns
                WHERE pattern_type IN (${filters.hasPatterns.map(() => '?').join(',')})
            )`);
            params.push(...filters.hasPatterns);
        }

        if (conditions.length > 0) {
            query += ' WHERE ' + conditions.join(' AND ');
        }

        query += ' ORDER BY created_at DESC LIMIT 100';

        return new Promise((resolve, reject) => {
            this.db.all(query, params, (err, rows) => {
                if (err) {
                    reject(err);
                } else {
                    resolve(rows.map(row => ({
                        ...row,
                        summary: JSON.parse(row.summary)
                    })));
                }
            });
        });
    }

    async searchTraces(pattern, searchType = 'paragraph') {
        let query;
        const params = [`%${pattern}%`];

        switch (searchType) {
            case 'paragraph':
                query = `
                    SELECT DISTINCT t.trace_id, t.program_name, t.created_at,
                           s.step_index, s.paragraph
                    FROM traces t
                    JOIN steps s ON t.trace_id = s.trace_id
                    WHERE s.paragraph LIKE ?
                    ORDER BY t.created_at DESC
                `;
                break;

            case 'variable':
                query = `
                    SELECT DISTINCT t.trace_id, t.program_name, t.created_at,
                           s.step_index, s.paragraph, s.variables
                    FROM traces t
                    JOIN steps s ON t.trace_id = s.trace_id
                    WHERE s.variables LIKE ?
                    ORDER BY t.created_at DESC
                `;
                break;

            case 'pattern':
                query = `
                    SELECT DISTINCT t.trace_id, t.program_name, t.created_at,
                           p.pattern_type, p.step_index, p.details
                    FROM traces t
                    JOIN patterns p ON t.trace_id = p.trace_id
                    WHERE p.pattern_type LIKE ?
                    ORDER BY t.created_at DESC
                `;
                break;

            default:
                throw new Error(`Invalid search type: ${searchType}`);
        }

        return new Promise((resolve, reject) => {
            this.db.all(query, params, (err, rows) => {
                if (err) {
                    reject(err);
                } else {
                    resolve(rows);
                }
            });
        });
    }

    async deleteTrace(traceId) {
        return new Promise((resolve, reject) => {
            this.db.serialize(() => {
                this.db.run('BEGIN TRANSACTION');

                try {
                    // Delete from all tables
                    this.db.run('DELETE FROM patterns WHERE trace_id = ?', [traceId]);
                    this.db.run('DELETE FROM steps WHERE trace_id = ?', [traceId]);
                    this.db.run('DELETE FROM traces WHERE trace_id = ?', [traceId]);

                    this.db.run('COMMIT', (err) => {
                        if (err) reject(err);
                        else resolve();
                    });
                } catch (error) {
                    this.db.run('ROLLBACK');
                    reject(error);
                }
            });
        });
    }

    async exportTraces(traceIds, format = 'json') {
        const traces = [];
        
        for (const traceId of traceIds) {
            const timeline = await this.loadTrace(traceId);
            traces.push(timeline);
        }

        switch (format) {
            case 'json':
                return JSON.stringify(traces.map(t => t.toJSON()), null, 2);
            
            case 'csv':
                return this.exportToCSV(traces);
            
            case 'html':
                return this.exportToHTML(traces);
            
            default:
                throw new Error(`Unsupported export format: ${format}`);
        }
    }

    exportToCSV(traces) {
        const csv = ['Trace ID,Program,Timestamp,Step,Paragraph,Section,Variables,Patterns'];
        
        for (const timeline of traces) {
            for (const step of timeline.steps) {
                csv.push([
                    timeline.traceId,
                    timeline.programName,
                    step.timestamp,
                    step.index,
                    step.paragraph,
                    step.section || '',
                    JSON.stringify(step.variables),
                    JSON.stringify(step.patterns)
                ].join(','));
            }
        }

        return csv.join('\n');
    }

    exportToHTML(traces) {
        // Generate HTML report
        return `
            <!DOCTYPE html>
            <html>
            <head>
                <title>COBOL Execution Traces</title>
                <style>
                    body { font-family: monospace; }
                    .trace { margin: 20px; padding: 10px; border: 1px solid #ccc; }
                    .step { margin: 5px 0; padding: 5px; background: #f0f0f0; }
                    .pattern { color: red; font-weight: bold; }
                </style>
            </head>
            <body>
                <h1>COBOL Execution Traces</h1>
                ${traces.map(t => this.traceToHTML(t)).join('')}
            </body>
            </html>
        `;
    }

    traceToHTML(timeline) {
        return `
            <div class="trace">
                <h2>${timeline.programName} - ${timeline.traceId}</h2>
                <p>Created: ${new Date(timeline.createdAt).toISOString()}</p>
                <p>Duration: ${timeline.duration}ms</p>
                <p>Steps: ${timeline.steps.length}</p>
                <div class="steps">
                    ${timeline.steps.slice(0, 100).map(s => this.stepToHTML(s)).join('')}
                </div>
            </div>
        `;
    }

    stepToHTML(step) {
        return `
            <div class="step">
                <strong>${step.index}:</strong> ${step.paragraph}
                ${step.patterns.length > 0 ? 
                    `<span class="pattern">[${step.patterns.map(p => p.type).join(', ')}]</span>` : 
                    ''}
            </div>
        `;
    }

    async getStatistics() {
        return new Promise((resolve, reject) => {
            const stats = {};

            this.db.serialize(() => {
                // Total traces
                this.db.get('SELECT COUNT(*) as count FROM traces', (err, row) => {
                    if (err) return reject(err);
                    stats.totalTraces = row.count;
                });

                // Total steps
                this.db.get('SELECT COUNT(*) as count FROM steps', (err, row) => {
                    if (err) return reject(err);
                    stats.totalSteps = row.count;
                });

                // Pattern statistics
                this.db.all(`
                    SELECT pattern_type, COUNT(*) as count
                    FROM patterns
                    GROUP BY pattern_type
                `, (err, rows) => {
                    if (err) return reject(err);
                    stats.patternCounts = rows;
                });

                // Program statistics
                this.db.all(`
                    SELECT program_name, COUNT(*) as count
                    FROM traces
                    GROUP BY program_name
                `, (err, rows) => {
                    if (err) return reject(err);
                    stats.programCounts = rows;
                    resolve(stats);
                });
            });
        });
    }

    close() {
        if (this.db) {
            this.db.close();
        }
    }
}

module.exports = TimelineStorage;