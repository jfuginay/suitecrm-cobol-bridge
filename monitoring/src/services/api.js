import axios from 'axios';

const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:3000/api';

class ApiService {
  constructor() {
    this.client = axios.create({
      baseURL: API_BASE_URL,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // Request interceptor
    this.client.interceptors.request.use(
      (config) => {
        // Add auth token if available
        const token = localStorage.getItem('authToken');
        if (token) {
          config.headers.Authorization = `Bearer ${token}`;
        }
        return config;
      },
      (error) => {
        return Promise.reject(error);
      }
    );

    // Response interceptor
    this.client.interceptors.response.use(
      (response) => response,
      (error) => {
        if (error.response?.status === 401) {
          // Handle unauthorized
          localStorage.removeItem('authToken');
          window.location.href = '/login';
        }
        return Promise.reject(error);
      }
    );
  }

  // Program monitoring endpoints
  async getProgramStatus() {
    const response = await this.client.get('/programs/status');
    return response.data;
  }

  async getProgramHistory(programId, limit = 100) {
    const response = await this.client.get(`/programs/${programId}/history`, {
      params: { limit }
    });
    return response.data;
  }

  // Batch job endpoints
  async getBatchJobs() {
    const response = await this.client.get('/batch/jobs');
    return response.data;
  }

  async getBatchJobDetails(jobId) {
    const response = await this.client.get(`/batch/jobs/${jobId}`);
    return response.data;
  }

  // Metrics endpoints
  async getMetrics(timeRange = '1h') {
    const response = await this.client.get('/metrics', {
      params: { timeRange }
    });
    return response.data;
  }

  async getSystemHealth() {
    const response = await this.client.get('/health');
    return response.data;
  }

  // Logs endpoints
  async getLogs(options = {}) {
    const response = await this.client.get('/logs', {
      params: {
        limit: options.limit || 100,
        offset: options.offset || 0,
        level: options.level,
        program: options.program,
        startTime: options.startTime,
        endTime: options.endTime
      }
    });
    return response.data;
  }

  // Alerts endpoints
  async getAlerts(status = 'active') {
    const response = await this.client.get('/alerts', {
      params: { status }
    });
    return response.data;
  }

  async acknowledgeAlert(alertId) {
    const response = await this.client.put(`/alerts/${alertId}/acknowledge`);
    return response.data;
  }

  async dismissAlert(alertId) {
    const response = await this.client.put(`/alerts/${alertId}/dismiss`);
    return response.data;
  }

  // Configuration endpoints
  async getConfiguration() {
    const response = await this.client.get('/config');
    return response.data;
  }

  async updateConfiguration(config) {
    const response = await this.client.put('/config', config);
    return response.data;
  }
}

export default new ApiService();