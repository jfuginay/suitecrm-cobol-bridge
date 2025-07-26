#!/usr/bin/env python3
"""
SuiteCRM COBOL Bridge - Runtime Wrapper Service
Provides HTTP API for COBOL program execution with monitoring and security
"""

import os
import sys
import json
import time
import uuid
import logging
import subprocess
import threading
import tempfile
import shutil
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional, Tuple

from flask import Flask, request, jsonify, Response
from flask_cors import CORS
import psutil

# Configuration
app = Flask(__name__)
CORS(app)

# Logging setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('cobol-wrapper')

# Runtime configuration
CONFIG = {
    'COBOL_BIN_PATH': os.environ.get('COBOL_BIN_PATH', '/compiled'),
    'TEMP_PATH': os.environ.get('TEMP_PATH', '/tmp/cobol-exec'),
    'MAX_EXECUTION_TIME': int(os.environ.get('MAX_EXECUTION_TIME', '300')),  # 5 minutes
    'MAX_FILE_SIZE': int(os.environ.get('MAX_FILE_SIZE', '104857600')),  # 100MB
    'ALLOWED_PROGRAMS': os.environ.get('ALLOWED_PROGRAMS', '').split(','),
    'ENABLE_DEBUG': os.environ.get('ENABLE_DEBUG', 'false').lower() == 'true',
    'MONITOR_PORT': int(os.environ.get('MONITOR_PORT', '5001')),
    'API_PORT': int(os.environ.get('API_PORT', '5000'))
}

# Global state
active_executions: Dict[str, Dict[str, Any]] = {}
execution_lock = threading.Lock()


class COBOLExecutor:
    """Handles COBOL program execution with monitoring and security"""
    
    def __init__(self, program_name: str, execution_id: str):
        self.program_name = program_name
        self.execution_id = execution_id
        self.temp_dir = None
        self.process = None
        self.start_time = None
        self.metrics = {
            'memory_usage': [],
            'cpu_usage': [],
            'io_operations': 0
        }
        
    def validate_program(self) -> Tuple[bool, Optional[str]]:
        """Validate program exists and is allowed"""
        # Check allowed programs list
        if CONFIG['ALLOWED_PROGRAMS'] and CONFIG['ALLOWED_PROGRAMS'][0]:
            if self.program_name not in CONFIG['ALLOWED_PROGRAMS']:
                return False, f"Program {self.program_name} is not in allowed list"
        
        # Check program exists
        program_path = Path(CONFIG['COBOL_BIN_PATH']) / self.program_name
        if not program_path.exists():
            return False, f"Program {self.program_name} not found"
            
        # Check if executable
        if not os.access(str(program_path), os.X_OK):
            return False, f"Program {self.program_name} is not executable"
            
        return True, None
        
    def create_temp_environment(self) -> Path:
        """Create secure temporary directory for execution"""
        self.temp_dir = Path(tempfile.mkdtemp(
            prefix=f"cobol-{self.execution_id}-",
            dir=CONFIG['TEMP_PATH']
        ))
        
        # Set restrictive permissions
        os.chmod(self.temp_dir, 0o700)
        
        # Create required files
        (self.temp_dir / 'input.dat').touch(mode=0o600)
        (self.temp_dir / 'output.dat').touch(mode=0o600)
        (self.temp_dir / 'monitor.log').touch(mode=0o600)
        
        if CONFIG['ENABLE_DEBUG']:
            (self.temp_dir / 'debug-trace.log').touch(mode=0o600)
            
        return self.temp_dir
        
    def prepare_environment(self) -> Dict[str, str]:
        """Prepare execution environment variables"""
        env = os.environ.copy()
        
        # COBOL runtime settings
        env.update({
            'COB_LIBRARY_PATH': '/usr/lib/gnucobol',
            'COB_CONFIG_DIR': '/usr/share/gnucobol/config',
            'COB_RUNTIME_CONFIG': 'default.conf',
            
            # File paths
            'COBOL_INPUT': str(self.temp_dir / 'input.dat'),
            'COBOL_OUTPUT': str(self.temp_dir / 'output.dat'),
            'MONITOR_FILE': str(self.temp_dir / 'monitor.log'),
            
            # Execution context
            'EXECUTION_ID': self.execution_id,
            'PROGRAM_NAME': self.program_name,
            
            # Security settings
            'COB_FILE_PATH': str(self.temp_dir),
            'COB_DISABLE_WARNINGS': 'N',
            
            # Performance settings
            'COB_PHYSICAL_CANCEL': 'Y',
            'COB_PRE_LOAD': 'Y'
        })
        
        if CONFIG['ENABLE_DEBUG']:
            env['DEBUG_TRACE'] = str(self.temp_dir / 'debug-trace.log')
            env['COB_SET_DEBUG'] = 'Y'
            env['COB_TRACE_FILE'] = str(self.temp_dir / 'debug-trace.log')
            
        # Remove potentially dangerous variables
        env.pop('LD_PRELOAD', None)
        
        return env
        
    def monitor_process(self):
        """Monitor process metrics in background"""
        while self.process and self.process.poll() is None:
            try:
                proc = psutil.Process(self.process.pid)
                
                # Collect metrics
                mem_info = proc.memory_info()
                cpu_percent = proc.cpu_percent(interval=0.1)
                
                metric = {
                    'timestamp': time.time(),
                    'memory_rss': mem_info.rss,
                    'memory_vms': mem_info.vms,
                    'cpu_percent': cpu_percent
                }
                
                self.metrics['memory_usage'].append(metric)
                
                # Send real-time update
                self._send_metric_update(metric)
                
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                break
                
            time.sleep(1)
            
    def _send_metric_update(self, metric: Dict[str, Any]):
        """Send metric update via monitoring channel"""
        # This would integrate with websocket or monitoring service
        logger.debug(f"Metric update for {self.execution_id}: {metric}")
        
    def execute(self, input_data: str) -> Dict[str, Any]:
        """Execute COBOL program with monitoring"""
        self.start_time = time.time()
        
        try:
            # Validate program
            valid, error = self.validate_program()
            if not valid:
                raise ValueError(error)
                
            # Create execution environment
            self.create_temp_environment()
            
            # Write input data
            input_path = self.temp_dir / 'input.dat'
            input_path.write_text(input_data)
            
            # Check input size
            if input_path.stat().st_size > CONFIG['MAX_FILE_SIZE']:
                raise ValueError(f"Input file too large: {input_path.stat().st_size} bytes")
                
            # Prepare environment
            env = self.prepare_environment()
            
            # Build command
            program_path = Path(CONFIG['COBOL_BIN_PATH']) / self.program_name
            
            # Start execution
            logger.info(f"Starting COBOL execution: {self.execution_id} - {self.program_name}")
            
            self.process = subprocess.Popen(
                [str(program_path)],
                cwd=str(self.temp_dir),
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True
            )
            
            # Start monitoring thread
            monitor_thread = threading.Thread(target=self.monitor_process)
            monitor_thread.daemon = True
            monitor_thread.start()
            
            # Wait for completion with timeout
            try:
                stdout, stderr = self.process.communicate(timeout=CONFIG['MAX_EXECUTION_TIME'])
                exit_code = self.process.returncode
            except subprocess.TimeoutExpired:
                self.process.kill()
                stdout, stderr = self.process.communicate()
                raise TimeoutError(f"Execution timeout after {CONFIG['MAX_EXECUTION_TIME']} seconds")
                
            # Read output
            output_path = self.temp_dir / 'output.dat'
            output_data = output_path.read_text() if output_path.exists() else ''
            
            # Read monitoring data
            monitor_path = self.temp_dir / 'monitor.log'
            monitor_data = monitor_path.read_text() if monitor_path.exists() else ''
            
            # Read debug trace if enabled
            debug_trace = None
            if CONFIG['ENABLE_DEBUG']:
                debug_path = self.temp_dir / 'debug-trace.log'
                if debug_path.exists():
                    debug_trace = debug_path.read_text()
                    
            # Calculate execution time
            execution_time = time.time() - self.start_time
            
            # Build result
            result = {
                'execution_id': self.execution_id,
                'program': self.program_name,
                'success': exit_code == 0,
                'exit_code': exit_code,
                'execution_time': execution_time,
                'output': output_data,
                'stdout': stdout,
                'stderr': stderr,
                'monitoring': self._parse_monitoring_data(monitor_data),
                'metrics': self.metrics,
                'timestamp': datetime.utcnow().isoformat() + 'Z'
            }
            
            if debug_trace:
                result['debug_trace'] = self._parse_debug_trace(debug_trace)
                
            logger.info(f"COBOL execution completed: {self.execution_id} - Time: {execution_time:.2f}s")
            
            return result
            
        except Exception as e:
            logger.error(f"COBOL execution failed: {self.execution_id} - {str(e)}")
            
            execution_time = time.time() - self.start_time if self.start_time else 0
            
            return {
                'execution_id': self.execution_id,
                'program': self.program_name,
                'success': False,
                'error': str(e),
                'execution_time': execution_time,
                'timestamp': datetime.utcnow().isoformat() + 'Z'
            }
            
        finally:
            # Cleanup
            self._cleanup()
            
    def _parse_monitoring_data(self, data: str) -> list:
        """Parse monitoring log data"""
        events = []
        for line in data.strip().split('\n'):
            if line:
                try:
                    events.append(json.loads(line))
                except json.JSONDecodeError:
                    events.append({'raw': line})
        return events
        
    def _parse_debug_trace(self, data: str) -> list:
        """Parse debug trace data"""
        traces = []
        for line in data.strip().split('\n'):
            if line:
                try:
                    traces.append(json.loads(line))
                except json.JSONDecodeError:
                    traces.append({'raw': line})
        return traces
        
    def _cleanup(self):
        """Clean up temporary resources"""
        if self.temp_dir and self.temp_dir.exists():
            try:
                shutil.rmtree(self.temp_dir)
            except Exception as e:
                logger.warning(f"Failed to cleanup temp dir: {e}")


# API Routes

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({
        'status': 'healthy',
        'service': 'cobol-wrapper',
        'timestamp': datetime.utcnow().isoformat() + 'Z',
        'active_executions': len(active_executions)
    })


@app.route('/runtime/status', methods=['GET'])
def runtime_status():
    """Get COBOL runtime status"""
    try:
        # Check cobc version
        result = subprocess.run(['cobc', '--version'], 
                              capture_output=True, text=True, timeout=5)
        
        version = result.stdout.split('\n')[0] if result.returncode == 0 else 'Unknown'
        
        # List available programs
        programs = []
        bin_path = Path(CONFIG['COBOL_BIN_PATH'])
        
        if bin_path.exists():
            for prog in bin_path.iterdir():
                if prog.is_file() and os.access(str(prog), os.X_OK):
                    # Check for metadata
                    meta_file = prog.with_suffix('.meta.json')
                    metadata = {}
                    if meta_file.exists():
                        try:
                            metadata = json.loads(meta_file.read_text())
                        except:
                            pass
                            
                    programs.append({
                        'name': prog.name,
                        'size': prog.stat().st_size,
                        'modified': prog.stat().st_mtime,
                        'metadata': metadata
                    })
                    
        return jsonify({
            'status': 'available',
            'runtime': {
                'version': version,
                'path': '/usr/bin/cobc'
            },
            'configuration': {
                'max_execution_time': CONFIG['MAX_EXECUTION_TIME'],
                'max_file_size': CONFIG['MAX_FILE_SIZE'],
                'debug_enabled': CONFIG['ENABLE_DEBUG']
            },
            'programs': programs
        })
        
    except Exception as e:
        return jsonify({
            'status': 'error',
            'error': str(e)
        }), 500


@app.route('/execute', methods=['POST'])
def execute_program():
    """Execute a COBOL program"""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'error': 'No data provided'}), 400
            
        program_name = data.get('program')
        input_data = data.get('input', '')
        
        if not program_name:
            return jsonify({'error': 'No program specified'}), 400
            
        # Generate execution ID
        execution_id = str(uuid.uuid4())
        
        # Track execution
        with execution_lock:
            active_executions[execution_id] = {
                'program': program_name,
                'start_time': time.time(),
                'status': 'running'
            }
            
        # Execute program
        executor = COBOLExecutor(program_name, execution_id)
        result = executor.execute(input_data)
        
        # Update tracking
        with execution_lock:
            if execution_id in active_executions:
                active_executions[execution_id]['status'] = 'completed'
                active_executions[execution_id]['result'] = result
                
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"Execution error: {str(e)}")
        return jsonify({
            'error': str(e),
            'type': type(e).__name__
        }), 500
        
    finally:
        # Cleanup old executions
        with execution_lock:
            cutoff = time.time() - 3600  # 1 hour
            to_remove = [eid for eid, info in active_executions.items() 
                        if info['start_time'] < cutoff]
            for eid in to_remove:
                del active_executions[eid]


@app.route('/execution/<execution_id>', methods=['GET'])
def get_execution_status(execution_id):
    """Get execution status"""
    with execution_lock:
        if execution_id not in active_executions:
            return jsonify({'error': 'Execution not found'}), 404
            
        execution = active_executions[execution_id].copy()
        
    return jsonify(execution)


@app.route('/compile', methods=['POST'])
def compile_program():
    """Compile a COBOL source file"""
    try:
        data = request.get_json()
        
        source_code = data.get('source')
        program_name = data.get('name', f'custom-{uuid.uuid4().hex[:8]}')
        
        if not source_code:
            return jsonify({'error': 'No source code provided'}), 400
            
        # Create temporary source file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cob', delete=False) as f:
            f.write(source_code)
            source_path = f.name
            
        try:
            # Compile
            output_path = Path(CONFIG['COBOL_BIN_PATH']) / program_name
            
            result = subprocess.run([
                'cobc', '-x', '-O2', '-Wall',
                '-o', str(output_path),
                source_path
            ], capture_output=True, text=True, timeout=60)
            
            if result.returncode != 0:
                return jsonify({
                    'success': False,
                    'error': result.stderr,
                    'output': result.stdout
                }), 400
                
            # Make executable
            os.chmod(output_path, 0o755)
            
            return jsonify({
                'success': True,
                'program': program_name,
                'output': result.stdout,
                'warnings': result.stderr
            })
            
        finally:
            # Cleanup
            Path(source_path).unlink(missing_ok=True)
            
    except Exception as e:
        logger.error(f"Compilation error: {str(e)}")
        return jsonify({
            'error': str(e),
            'type': type(e).__name__
        }), 500


@app.route('/programs', methods=['GET'])
def list_programs():
    """List available COBOL programs"""
    programs = []
    bin_path = Path(CONFIG['COBOL_BIN_PATH'])
    
    if bin_path.exists():
        for prog in bin_path.iterdir():
            if prog.is_file() and os.access(str(prog), os.X_OK):
                programs.append({
                    'name': prog.name,
                    'size': prog.stat().st_size,
                    'modified': datetime.fromtimestamp(prog.stat().st_mtime).isoformat() + 'Z'
                })
                
    return jsonify({
        'programs': programs,
        'count': len(programs)
    })


def initialize():
    """Initialize the service"""
    # Create required directories
    Path(CONFIG['TEMP_PATH']).mkdir(parents=True, exist_ok=True)
    Path(CONFIG['COBOL_BIN_PATH']).mkdir(parents=True, exist_ok=True)
    
    # Log configuration
    logger.info("COBOL Wrapper Service starting...")
    logger.info(f"Configuration: {CONFIG}")
    
    # Check COBOL runtime
    try:
        result = subprocess.run(['cobc', '--version'], 
                              capture_output=True, text=True, timeout=5)
        if result.returncode == 0:
            logger.info(f"COBOL runtime: {result.stdout.split()[0]}")
        else:
            logger.warning("COBOL runtime check failed")
    except Exception as e:
        logger.error(f"Failed to check COBOL runtime: {e}")


if __name__ == '__main__':
    initialize()
    
    # Start Flask app
    app.run(
        host='0.0.0.0',
        port=CONFIG['API_PORT'],
        debug=CONFIG['ENABLE_DEBUG'],
        threaded=True
    )