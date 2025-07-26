import React from 'react';
import styled from 'styled-components';
import { FiClock, FiCheckCircle, FiAlertCircle, FiLoader } from 'react-icons/fi';
import { format, formatDistance } from 'date-fns';

const Container = styled.div`
  display: flex;
  flex-direction: column;
  gap: 1rem;
  flex: 1;
  overflow-y: auto;
`;

const JobCard = styled.div`
  background-color: #0f1419;
  border: 1px solid #2f3741;
  border-radius: 6px;
  padding: 1rem;
  transition: all 0.2s ease;

  &:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  }
`;

const JobHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
`;

const JobInfo = styled.div`
  flex: 1;
`;

const JobName = styled.h3`
  margin: 0;
  font-size: 0.95rem;
  font-weight: 600;
  color: #fff;
`;

const JobDetails = styled.div`
  display: flex;
  gap: 1rem;
  margin-top: 0.25rem;
  font-size: 0.75rem;
  color: #9ca3af;
`;

const StatusIcon = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  width: 32px;
  height: 32px;
  border-radius: 50%;
  background-color: ${props => {
    switch(props.$status) {
      case 'running': return 'rgba(16, 185, 129, 0.2)';
      case 'completed': return 'rgba(16, 185, 129, 0.2)';
      case 'failed': return 'rgba(239, 68, 68, 0.2)';
      case 'pending': return 'rgba(107, 114, 128, 0.2)';
      default: return 'rgba(107, 114, 128, 0.2)';
    }
  }};
  
  svg {
    color: ${props => {
      switch(props.$status) {
        case 'running': return '#10b981';
        case 'completed': return '#10b981';
        case 'failed': return '#ef4444';
        case 'pending': return '#6b7280';
        default: return '#6b7280';
      }
    }};
    
    ${props => props.$status === 'running' && `
      animation: spin 2s linear infinite;
    `}
  }

  @keyframes spin {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
  }
`;

const ProgressContainer = styled.div`
  margin-top: 0.75rem;
`;

const ProgressInfo = styled.div`
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.5rem;
  font-size: 0.875rem;
`;

const ProgressText = styled.span`
  color: #e5e7eb;
`;

const ProgressBar = styled.div`
  width: 100%;
  height: 8px;
  background-color: #2f3741;
  border-radius: 4px;
  overflow: hidden;
  position: relative;
`;

const ProgressFill = styled.div`
  height: 100%;
  background: ${props => {
    if (props.$status === 'failed') return '#ef4444';
    if (props.$status === 'completed') return '#10b981';
    return 'linear-gradient(90deg, #10b981 0%, #34d399 100%)';
  }};
  width: ${props => props.$progress}%;
  transition: width 0.3s ease;
  position: relative;
  
  ${props => props.$status === 'running' && `
    &::after {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      bottom: 0;
      right: 0;
      background: linear-gradient(
        90deg,
        transparent,
        rgba(255, 255, 255, 0.1),
        transparent
      );
      animation: shimmer 1.5s infinite;
    }
  `}

  @keyframes shimmer {
    0% { transform: translateX(-100%); }
    100% { transform: translateX(100%); }
  }
`;

const StatsRow = styled.div`
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 1rem;
  margin-top: 0.75rem;
  padding-top: 0.75rem;
  border-top: 1px solid #2f3741;
`;

const Stat = styled.div`
  text-align: center;
`;

const StatValue = styled.div`
  font-size: 1.25rem;
  font-weight: 600;
  color: #fff;
`;

const StatLabel = styled.div`
  font-size: 0.75rem;
  color: #9ca3af;
  margin-top: 0.25rem;
`;

const NoJobs = styled.div`
  text-align: center;
  padding: 3rem;
  color: #6b7280;
`;

function BatchJobProgress({ jobs = [] }) {
  const getStatusIcon = (status) => {
    switch(status) {
      case 'running':
        return <FiLoader size={18} />;
      case 'completed':
        return <FiCheckCircle size={18} />;
      case 'failed':
        return <FiAlertCircle size={18} />;
      default:
        return <FiClock size={18} />;
    }
  };

  const formatDuration = (seconds) => {
    if (!seconds) return '0s';
    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = seconds % 60;
    
    if (hours > 0) {
      return `${hours}h ${minutes}m`;
    } else if (minutes > 0) {
      return `${minutes}m ${secs}s`;
    }
    return `${secs}s`;
  };

  if (jobs.length === 0) {
    return (
      <Container>
        <NoJobs>No batch jobs are currently running</NoJobs>
      </Container>
    );
  }

  return (
    <Container>
      {jobs.map((job) => (
        <JobCard key={job.id}>
          <JobHeader>
            <JobInfo>
              <JobName>{job.name}</JobName>
              <JobDetails>
                <span>ID: {job.id}</span>
                <span>•</span>
                <span>Started: {job.startTime ? format(new Date(job.startTime), 'HH:mm:ss') : 'N/A'}</span>
              </JobDetails>
            </JobInfo>
            <StatusIcon $status={job.status}>
              {getStatusIcon(job.status)}
            </StatusIcon>
          </JobHeader>

          <ProgressContainer>
            <ProgressInfo>
              <ProgressText>
                {job.processedRecords || 0} / {job.totalRecords || 0} records
              </ProgressText>
              <ProgressText>
                {Math.round(job.progress || 0)}%
              </ProgressText>
            </ProgressInfo>
            <ProgressBar>
              <ProgressFill 
                $progress={job.progress || 0} 
                $status={job.status}
              />
            </ProgressBar>
          </ProgressContainer>

          <StatsRow>
            <Stat>
              <StatValue>{formatDuration(job.elapsedTime || 0)}</StatValue>
              <StatLabel>Elapsed Time</StatLabel>
            </Stat>
            <Stat>
              <StatValue>{job.recordsPerSecond || 0}</StatValue>
              <StatLabel>Records/sec</StatLabel>
            </Stat>
            <Stat>
              <StatValue>{formatDuration(job.estimatedTimeRemaining || 0)}</StatValue>
              <StatLabel>Est. Remaining</StatLabel>
            </Stat>
          </StatsRow>
        </JobCard>
      ))}
    </Container>
  );
}

export default BatchJobProgress;