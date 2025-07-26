#!/usr/bin/env python3
"""
SuiteCRM COBOL Bridge - Monitoring Agent
Collects and exposes system and COBOL execution metrics
"""

import os
import time
import json
import threading
import psutil
from datetime import datetime
from pathlib import Path
from prometheus_client import Counter, Histogram, Gauge, start_http_server
from flask import Flask, jsonify
import logging

# Configuration
MONITOR_PORT = int(os.environ.get('MONITOR_PORT', '5001'))
METRICS_INTERVAL = int(os.environ.get('METRICS_INTERVAL', '5'))
LOG_PATH = Path(os.environ.get('LOG_PATH', '/logs'))

# Logging setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('monitor-agent')

# Prometheus metrics
cobol_executions_total = Counter('cobol_executions_total', 
                                'Total number of COBOL program executions',
                                ['program', 'status'])

cobol_execution_duration = Histogram('cobol_execution_duration_seconds',
                                   'COBOL program execution duration',
                                   ['program'],
                                   buckets=(0.1, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0, 60.0))

system_cpu_usage = Gauge('system_cpu_usage_percent', 
                        'System CPU usage percentage')

system_memory_usage = Gauge('system_memory_usage_bytes',
                           'System memory usage in bytes',
                           ['type'])

system_disk_usage = Gauge('system_disk_usage_bytes',
                         'System disk usage in bytes',
                         ['mount', 'type'])

active_cobol_processes = Gauge('active_cobol_processes',
                              'Number of active COBOL processes')

cobol_io_operations = Counter('cobol_io_operations_total',
                             'Total I/O operations by COBOL programs',
                             ['operation'])

# Flask app for custom metrics endpoint
app = Flask(__name__)


class SystemMonitor:
    """Monitors system resources"""
    
    def __init__(self):
        self.running = True
        self.thread = threading.Thread(target=self._monitor_loop)
        self.thread.daemon = True
        
    def start(self):
        """Start monitoring"""
        self.thread.start()
        logger.info("System monitoring started")
        
    def stop(self):
        """Stop monitoring"""
        self.running = False
        self.thread.join()
        
    def _monitor_loop(self):
        """Main monitoring loop"""
        while self.running:
            try:
                # CPU usage
                cpu_percent = psutil.cpu_percent(interval=1)
                system_cpu_usage.set(cpu_percent)
                
                # Memory usage
                memory = psutil.virtual_memory()
                system_memory_usage.labels(type='used').set(memory.used)
                system_memory_usage.labels(type='available').set(memory.available)
                system_memory_usage.labels(type='total').set(memory.total)
                
                # Disk usage
                for partition in psutil.disk_partitions():
                    try:
                        usage = psutil.disk_usage(partition.mountpoint)
                        system_disk_usage.labels(
                            mount=partition.mountpoint,
                            type='used'
                        ).set(usage.used)
                        system_disk_usage.labels(
                            mount=partition.mountpoint,
                            type='free'
                        ).set(usage.free)
                    except PermissionError:
                        continue
                        
                # Count COBOL processes
                cobol_count = sum(1 for p in psutil.process_iter(['name']) 
                                 if p.info['name'] and 'cobol' in p.info['name'].lower())
                active_cobol_processes.set(cobol_count)
                
                time.sleep(METRICS_INTERVAL)
                
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}")
                time.sleep(METRICS_INTERVAL)


class ExecutionMonitor:
    """Monitors COBOL program executions"""
    
    def __init__(self):
        self.executions = {}
        self.lock = threading.Lock()
        
    def start_execution(self, execution_id, program_name):
        """Record execution start"""
        with self.lock:
            self.executions[execution_id] = {
                'program': program_name,
                'start_time': time.time(),
                'metrics': {
                    'io_reads': 0,
                    'io_writes': 0,
                    'memory_peak': 0
                }
            }
            
    def update_execution(self, execution_id, metrics):
        """Update execution metrics"""
        with self.lock:
            if execution_id in self.executions:
                self.executions[execution_id]['metrics'].update(metrics)
                
    def end_execution(self, execution_id, success=True):
        """Record execution end"""
        with self.lock:
            if execution_id in self.executions:
                execution = self.executions[execution_id]
                duration = time.time() - execution['start_time']
                
                # Update Prometheus metrics
                status = 'success' if success else 'failure'
                cobol_executions_total.labels(
                    program=execution['program'],
                    status=status
                ).inc()
                
                cobol_execution_duration.labels(
                    program=execution['program']
                ).observe(duration)
                
                # Update I/O metrics
                cobol_io_operations.labels(operation='read').inc(
                    execution['metrics'].get('io_reads', 0)
                )
                cobol_io_operations.labels(operation='write').inc(
                    execution['metrics'].get('io_writes', 0)
                )
                
                # Log execution summary
                logger.info(f"Execution {execution_id} completed: "
                          f"program={execution['program']}, "
                          f"duration={duration:.2f}s, "
                          f"status={status}")
                
                # Clean up
                del self.executions[execution_id]


class LogMonitor:
    """Monitors log files for events"""
    
    def __init__(self, execution_monitor):
        self.execution_monitor = execution_monitor
        self.monitors = {}
        self.running = True
        self.thread = threading.Thread(target=self._monitor_logs)
        self.thread.daemon = True
        
    def start(self):
        """Start log monitoring"""
        self.thread.start()
        logger.info("Log monitoring started")
        
    def stop(self):
        """Stop log monitoring"""
        self.running = False
        self.thread.join()
        
    def _monitor_logs(self):
        """Monitor log files for COBOL execution events"""
        while self.running:
            try:
                # Find monitor log files
                monitor_files = list(Path('/tmp/cobol-exec').glob('*/monitor.log'))
                
                for log_file in monitor_files:
                    if log_file not in self.monitors:
                        self.monitors[log_file] = {
                            'position': 0,
                            'execution_id': log_file.parent.name
                        }
                        
                    # Read new entries
                    monitor = self.monitors[log_file]
                    try:
                        with open(log_file, 'r') as f:
                            f.seek(monitor['position'])
                            for line in f:
                                self._process_log_entry(monitor['execution_id'], line)
                            monitor['position'] = f.tell()
                    except Exception as e:
                        logger.debug(f"Error reading log {log_file}: {e}")
                        
                # Clean up old monitors
                self._cleanup_monitors()
                
                time.sleep(1)
                
            except Exception as e:
                logger.error(f"Error in log monitoring: {e}")
                time.sleep(5)
                
    def _process_log_entry(self, execution_id, line):
        """Process a single log entry"""
        try:
            entry = json.loads(line.strip())
            event = entry.get('event')
            
            if event == 'START':
                self.execution_monitor.start_execution(
                    execution_id,
                    entry.get('program', 'unknown')
                )
            elif event == 'END':
                self.execution_monitor.end_execution(
                    execution_id,
                    entry.get('status') == 'SUCCESS'
                )
            elif event == 'METRIC':
                self.execution_monitor.update_execution(
                    execution_id,
                    entry.get('data', {})
                )
                
        except json.JSONDecodeError:
            pass  # Ignore non-JSON lines
            
    def _cleanup_monitors(self):
        """Remove monitors for deleted files"""
        existing = set(self.monitors.keys())
        for log_file in existing:
            if not log_file.exists():
                del self.monitors[log_file]


# Flask endpoints

@app.route('/metrics/json')
def metrics_json():
    """Export metrics as JSON"""
    metrics = {
        'timestamp': datetime.utcnow().isoformat() + 'Z',
        'system': {
            'cpu_percent': psutil.cpu_percent(),
            'memory': dict(psutil.virtual_memory()._asdict()),
            'disk': {}
        },
        'cobol': {
            'active_processes': active_cobol_processes._value.get(),
            'total_executions': sum(
                cobol_executions_total.labels(program=p, status=s)._value.get()
                for p in ['financial-calc', 'payroll', 'credit']
                for s in ['success', 'failure']
            )
        }
    }
    
    # Add disk usage
    for partition in psutil.disk_partitions():
        try:
            usage = psutil.disk_usage(partition.mountpoint)
            metrics['system']['disk'][partition.mountpoint] = dict(usage._asdict())
        except PermissionError:
            continue
            
    return jsonify(metrics)


@app.route('/health')
def health():
    """Health check endpoint"""
    return jsonify({
        'status': 'healthy',
        'service': 'monitor-agent',
        'timestamp': datetime.utcnow().isoformat() + 'Z'
    })


def main():
    """Main entry point"""
    logger.info("Starting COBOL monitoring agent...")
    
    # Create log directory
    LOG_PATH.mkdir(parents=True, exist_ok=True)
    
    # Start Prometheus metrics server
    start_http_server(MONITOR_PORT)
    logger.info(f"Prometheus metrics available on port {MONITOR_PORT}")
    
    # Initialize monitors
    system_monitor = SystemMonitor()
    execution_monitor = ExecutionMonitor()
    log_monitor = LogMonitor(execution_monitor)
    
    # Start monitoring
    system_monitor.start()
    log_monitor.start()
    
    # Start Flask app in separate thread
    flask_thread = threading.Thread(
        target=lambda: app.run(host='0.0.0.0', port=MONITOR_PORT + 1)
    )
    flask_thread.daemon = True
    flask_thread.start()
    
    logger.info("Monitoring agent started successfully")
    
    try:
        # Keep running
        while True:
            time.sleep(60)
    except KeyboardInterrupt:
        logger.info("Shutting down monitoring agent...")
        system_monitor.stop()
        log_monitor.stop()


if __name__ == '__main__':
    main()