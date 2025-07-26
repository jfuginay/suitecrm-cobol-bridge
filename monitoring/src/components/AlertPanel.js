import React from 'react';
import styled from 'styled-components';
import { FiAlertCircle, FiAlertTriangle, FiInfo, FiX, FiCheck } from 'react-icons/fi';
import { formatDistance } from 'date-fns';

const Container = styled.div`
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  flex: 1;
  overflow-y: auto;
`;

const Alert = styled.div`
  background-color: #0f1419;
  border: 1px solid ${props => {
    switch(props.$severity) {
      case 'critical': return '#ef4444';
      case 'warning': return '#f59e0b';
      case 'info': return '#3b82f6';
      default: return '#2f3741';
    }
  }};
  border-radius: 6px;
  padding: 1rem;
  position: relative;
  opacity: ${props => props.$acknowledged ? 0.6 : 1};
`;

const AlertHeader = styled.div`
  display: flex;
  align-items: flex-start;
  gap: 0.75rem;
`;

const AlertIcon = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  flex-shrink: 0;
  background-color: ${props => {
    switch(props.$severity) {
      case 'critical': return 'rgba(239, 68, 68, 0.2)';
      case 'warning': return 'rgba(245, 158, 11, 0.2)';
      case 'info': return 'rgba(59, 130, 246, 0.2)';
      default: return 'rgba(107, 114, 128, 0.2)';
    }
  }};
  
  svg {
    color: ${props => {
      switch(props.$severity) {
        case 'critical': return '#ef4444';
        case 'warning': return '#f59e0b';
        case 'info': return '#3b82f6';
        default: return '#6b7280';
      }
    }};
  }
`;

const AlertContent = styled.div`
  flex: 1;
`;

const AlertTitle = styled.h4`
  margin: 0 0 0.25rem 0;
  font-size: 0.875rem;
  font-weight: 600;
  color: #fff;
`;

const AlertMessage = styled.p`
  margin: 0 0 0.5rem 0;
  font-size: 0.8125rem;
  color: #e5e7eb;
  line-height: 1.5;
`;

const AlertMeta = styled.div`
  display: flex;
  gap: 1rem;
  font-size: 0.75rem;
  color: #9ca3af;
`;

const AlertActions = styled.div`
  display: flex;
  gap: 0.5rem;
`;

const ActionButton = styled.button`
  display: flex;
  align-items: center;
  justify-content: center;
  width: 28px;
  height: 28px;
  border-radius: 4px;
  border: 1px solid #2f3741;
  background-color: transparent;
  color: #9ca3af;
  cursor: pointer;
  transition: all 0.2s ease;

  &:hover {
    background-color: #1a1f29;
    color: #e5e7eb;
    border-color: #394251;
  }

  &:active {
    transform: scale(0.95);
  }
`;

const NoAlerts = styled.div`
  text-align: center;
  padding: 2rem;
  color: #6b7280;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
`;

const NoAlertsIcon = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  width: 48px;
  height: 48px;
  border-radius: 50%;
  background-color: rgba(16, 185, 129, 0.1);
  color: #10b981;
  margin-bottom: 0.5rem;
`;

function AlertPanel({ alerts = [], onAcknowledge, onDismiss }) {
  const getAlertIcon = (severity) => {
    switch(severity) {
      case 'critical':
        return <FiAlertCircle size={14} />;
      case 'warning':
        return <FiAlertTriangle size={14} />;
      case 'info':
        return <FiInfo size={14} />;
      default:
        return <FiInfo size={14} />;
    }
  };

  if (alerts.length === 0) {
    return (
      <Container>
        <NoAlerts>
          <NoAlertsIcon>
            <FiCheck size={24} />
          </NoAlertsIcon>
          <div>No active alerts</div>
          <div style={{ fontSize: '0.75rem', color: '#6b7280' }}>
            All systems operating normally
          </div>
        </NoAlerts>
      </Container>
    );
  }

  return (
    <Container>
      {alerts.map((alert) => (
        <Alert key={alert.id} $severity={alert.severity} $acknowledged={alert.acknowledged}>
          <AlertHeader>
            <AlertIcon $severity={alert.severity}>
              {getAlertIcon(alert.severity)}
            </AlertIcon>
            <AlertContent>
              <AlertTitle>{alert.title}</AlertTitle>
              <AlertMessage>{alert.message}</AlertMessage>
              <AlertMeta>
                <span>{alert.source}</span>
                <span>•</span>
                <span>
                  {formatDistance(new Date(alert.timestamp), new Date(), { addSuffix: true })}
                </span>
              </AlertMeta>
            </AlertContent>
            <AlertActions>
              {!alert.acknowledged && (
                <ActionButton
                  onClick={() => onAcknowledge(alert.id)}
                  title="Acknowledge alert"
                >
                  <FiCheck size={14} />
                </ActionButton>
              )}
              <ActionButton
                onClick={() => onDismiss(alert.id)}
                title="Dismiss alert"
              >
                <FiX size={14} />
              </ActionButton>
            </AlertActions>
          </AlertHeader>
        </Alert>
      ))}
    </Container>
  );
}

export default AlertPanel;