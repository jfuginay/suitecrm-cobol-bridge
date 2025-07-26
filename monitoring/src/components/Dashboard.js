import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import ProgramMonitor from './ProgramMonitor';
import BatchJobProgress from './BatchJobProgress';
import MetricsChart from './MetricsChart';
import LogViewer from './LogViewer';
import AlertPanel from './AlertPanel';
import api from '../services/api';
import websocket from '../services/websocket';

const DashboardContainer = styled.div`
  padding: 1.5rem;
  display: grid;
  grid-template-columns: repeat(12, 1fr);
  grid-template-rows: auto auto 1fr;
  gap: 1.5rem;
  min-height: calc(100vh - 65px);
`;

const Section = styled.section`
  background-color: #1a1f29;
  border: 1px solid #2f3741;
  border-radius: 8px;
  padding: 1.5rem;
  overflow: hidden;
  display: flex;
  flex-direction: column;
`;

const SectionTitle = styled.h2`
  margin: 0 0 1rem 0;
  font-size: 1.125rem;
  font-weight: 600;
  color: #fff;
  display: flex;
  align-items: center;
  gap: 0.5rem;
`;

const ProgramSection = styled(Section)`
  grid-column: 1 / 7;
  grid-row: 1;
`;

const BatchSection = styled(Section)`
  grid-column: 7 / 13;
  grid-row: 1;
`;

const MetricsSection = styled(Section)`
  grid-column: 1 / 9;
  grid-row: 2;
`;

const AlertSection = styled(Section)`
  grid-column: 9 / 13;
  grid-row: 2;
  max-height: 400px;
`;

const LogSection = styled(Section)`
  grid-column: 1 / 13;
  grid-row: 3;
  min-height: 300px;
`;

const LoadingMessage = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 2rem;
  color: #6b7280;
`;

function Dashboard() {
  const [programs, setPrograms] = useState([]);
  const [batchJobs, setBatchJobs] = useState([]);
  const [metrics, setMetrics] = useState(null);
  const [alerts, setAlerts] = useState([]);
  const [logs, setLogs] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    // Initial data load
    loadInitialData();

    // Set up WebSocket listeners
    const handleProgramStatus = (data) => {
      setPrograms(prev => {
        const index = prev.findIndex(p => p.id === data.id);
        if (index >= 0) {
          const updated = [...prev];
          updated[index] = { ...updated[index], ...data };
          return updated;
        }
        return [...prev, data];
      });
    };

    const handleBatchProgress = (data) => {
      setBatchJobs(prev => {
        const index = prev.findIndex(j => j.id === data.id);
        if (index >= 0) {
          const updated = [...prev];
          updated[index] = { ...updated[index], ...data };
          return updated;
        }
        return [...prev, data];
      });
    };

    const handleMetricsUpdate = (data) => {
      setMetrics(data);
    };

    const handleNewLog = (data) => {
      setLogs(prev => [data, ...prev].slice(0, 100)); // Keep last 100 logs
    };

    const handleNewAlert = (data) => {
      setAlerts(prev => [data, ...prev]);
    };

    websocket.on('program:status', handleProgramStatus);
    websocket.on('batch:progress', handleBatchProgress);
    websocket.on('metrics:update', handleMetricsUpdate);
    websocket.on('log:new', handleNewLog);
    websocket.on('alert:new', handleNewAlert);

    return () => {
      websocket.off('program:status', handleProgramStatus);
      websocket.off('batch:progress', handleBatchProgress);
      websocket.off('metrics:update', handleMetricsUpdate);
      websocket.off('log:new', handleNewLog);
      websocket.off('alert:new', handleNewAlert);
    };
  }, []);

  const loadInitialData = async () => {
    try {
      setLoading(true);
      const [programsData, jobsData, metricsData, alertsData, logsData] = await Promise.all([
        api.getProgramStatus(),
        api.getBatchJobs(),
        api.getMetrics(),
        api.getAlerts(),
        api.getLogs()
      ]);

      setPrograms(programsData.programs || []);
      setBatchJobs(jobsData.jobs || []);
      setMetrics(metricsData);
      setAlerts(alertsData.alerts || []);
      setLogs(logsData.logs || []);
    } catch (error) {
      console.error('Failed to load initial data:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleAlertAcknowledge = async (alertId) => {
    try {
      await api.acknowledgeAlert(alertId);
      setAlerts(prev => prev.map(alert => 
        alert.id === alertId ? { ...alert, acknowledged: true } : alert
      ));
    } catch (error) {
      console.error('Failed to acknowledge alert:', error);
    }
  };

  const handleAlertDismiss = async (alertId) => {
    try {
      await api.dismissAlert(alertId);
      setAlerts(prev => prev.filter(alert => alert.id !== alertId));
    } catch (error) {
      console.error('Failed to dismiss alert:', error);
    }
  };

  if (loading) {
    return (
      <DashboardContainer>
        <LoadingMessage>Loading dashboard data...</LoadingMessage>
      </DashboardContainer>
    );
  }

  return (
    <DashboardContainer>
      <ProgramSection>
        <SectionTitle>COBOL Programs</SectionTitle>
        <ProgramMonitor programs={programs} />
      </ProgramSection>

      <BatchSection>
        <SectionTitle>Batch Jobs</SectionTitle>
        <BatchJobProgress jobs={batchJobs} />
      </BatchSection>

      <MetricsSection>
        <SectionTitle>Performance Metrics</SectionTitle>
        <MetricsChart metrics={metrics} />
      </MetricsSection>

      <AlertSection>
        <SectionTitle>Alerts</SectionTitle>
        <AlertPanel 
          alerts={alerts}
          onAcknowledge={handleAlertAcknowledge}
          onDismiss={handleAlertDismiss}
        />
      </AlertSection>

      <LogSection>
        <SectionTitle>Live Logs</SectionTitle>
        <LogViewer logs={logs} />
      </LogSection>
    </DashboardContainer>
  );
}

export default Dashboard;