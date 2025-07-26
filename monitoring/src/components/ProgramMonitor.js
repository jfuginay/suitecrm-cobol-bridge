import React from 'react';
import styled from 'styled-components';
import { FiActivity, FiClock, FiCpu, FiDatabase } from 'react-icons/fi';
import { format } from 'date-fns';

const Container = styled.div`
  display: flex;
  flex-direction: column;
  gap: 1rem;
  flex: 1;
  overflow-y: auto;
`;

const ProgramCard = styled.div`
  background-color: #0f1419;
  border: 1px solid ${props => {
    switch(props.$status) {
      case 'running': return '#10b981';
      case 'error': return '#ef4444';
      case 'warning': return '#f59e0b';
      default: return '#2f3741';
    }
  }};
  border-radius: 6px;
  padding: 1rem;
  transition: all 0.2s ease;

  &:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  }
`;

const ProgramHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.75rem;
`;

const ProgramName = styled.h3`
  margin: 0;
  font-size: 1rem;
  font-weight: 600;
  color: #fff;
`;

const StatusBadge = styled.span`
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.75rem;
  font-weight: 500;
  background-color: ${props => {
    switch(props.$status) {
      case 'running': return 'rgba(16, 185, 129, 0.2)';
      case 'stopped': return 'rgba(107, 114, 128, 0.2)';
      case 'error': return 'rgba(239, 68, 68, 0.2)';
      case 'warning': return 'rgba(245, 158, 11, 0.2)';
      default: return 'rgba(107, 114, 128, 0.2)';
    }
  }};
  color: ${props => {
    switch(props.$status) {
      case 'running': return '#10b981';
      case 'stopped': return '#6b7280';
      case 'error': return '#ef4444';
      case 'warning': return '#f59e0b';
      default: return '#6b7280';
    }
  }};
`;

const MetricsRow = styled.div`
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 1rem;
`;

const Metric = styled.div`
  display: flex;
  align-items: center;
  gap: 0.5rem;
  color: #9ca3af;
  font-size: 0.875rem;

  svg {
    color: #6b7280;
  }
`;

const MetricValue = styled.span`
  color: #e5e7eb;
  font-weight: 500;
`;

const ProgressBar = styled.div`
  width: 100%;
  height: 4px;
  background-color: #2f3741;
  border-radius: 2px;
  margin-top: 0.75rem;
  overflow: hidden;
`;

const ProgressFill = styled.div`
  height: 100%;
  background-color: ${props => props.$status === 'error' ? '#ef4444' : '#10b981'};
  width: ${props => props.$progress}%;
  transition: width 0.3s ease;
`;

const NoPrograms = styled.div`
  text-align: center;
  padding: 3rem;
  color: #6b7280;
`;

function ProgramMonitor({ programs = [] }) {
  const getStatusColor = (status) => {
    switch(status) {
      case 'running': return '#10b981';
      case 'error': return '#ef4444';
      case 'warning': return '#f59e0b';
      default: return '#6b7280';
    }
  };

  if (programs.length === 0) {
    return (
      <Container>
        <NoPrograms>No programs are currently being monitored</NoPrograms>
      </Container>
    );
  }

  return (
    <Container>
      {programs.map((program) => (
        <ProgramCard key={program.id} $status={program.status}>
          <ProgramHeader>
            <ProgramName>{program.name}</ProgramName>
            <StatusBadge $status={program.status}>
              {program.status.toUpperCase()}
            </StatusBadge>
          </ProgramHeader>
          
          <MetricsRow>
            <Metric>
              <FiClock size={16} />
              <span>Runtime: <MetricValue>{program.runtime || '0s'}</MetricValue></span>
            </Metric>
            <Metric>
              <FiCpu size={16} />
              <span>CPU: <MetricValue>{program.cpuUsage || '0'}%</MetricValue></span>
            </Metric>
            <Metric>
              <FiDatabase size={16} />
              <span>Memory: <MetricValue>{program.memoryUsage || '0'}MB</MetricValue></span>
            </Metric>
            <Metric>
              <FiActivity size={16} />
              <span>Calls: <MetricValue>{program.callCount || '0'}</MetricValue></span>
            </Metric>
          </MetricsRow>

          {program.progress !== undefined && (
            <ProgressBar>
              <ProgressFill 
                $progress={program.progress} 
                $status={program.status}
              />
            </ProgressBar>
          )}
        </ProgramCard>
      ))}
    </Container>
  );
}

export default ProgramMonitor;