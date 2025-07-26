import React, { useRef, useEffect, useState } from 'react';
import styled from 'styled-components';
import { FiFilter, FiDownload, FiMaximize2 } from 'react-icons/fi';
import { format } from 'date-fns';

const Container = styled.div`
  display: flex;
  flex-direction: column;
  flex: 1;
  min-height: 0;
`;

const LogHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  gap: 1rem;
`;

const FilterGroup = styled.div`
  display: flex;
  gap: 0.5rem;
  align-items: center;
  flex: 1;
`;

const FilterSelect = styled.select`
  padding: 0.5rem;
  border-radius: 4px;
  border: 1px solid #2f3741;
  background-color: #0f1419;
  color: #e5e7eb;
  font-size: 0.875rem;
  cursor: pointer;

  &:focus {
    outline: none;
    border-color: #10b981;
  }
`;

const SearchInput = styled.input`
  flex: 1;
  padding: 0.5rem;
  border-radius: 4px;
  border: 1px solid #2f3741;
  background-color: #0f1419;
  color: #e5e7eb;
  font-size: 0.875rem;

  &:focus {
    outline: none;
    border-color: #10b981;
  }

  &::placeholder {
    color: #6b7280;
  }
`;

const ActionGroup = styled.div`
  display: flex;
  gap: 0.5rem;
`;

const ActionButton = styled.button`
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 0.75rem;
  border-radius: 4px;
  border: 1px solid #2f3741;
  background-color: transparent;
  color: #9ca3af;
  font-size: 0.875rem;
  cursor: pointer;
  transition: all 0.2s ease;

  &:hover {
    background-color: #1a1f29;
    color: #e5e7eb;
    border-color: #394251;
  }
`;

const LogContainer = styled.div`
  flex: 1;
  overflow-y: auto;
  background-color: #0f1419;
  border: 1px solid #2f3741;
  border-radius: 4px;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-size: 0.8125rem;
  line-height: 1.5;
`;

const LogEntry = styled.div`
  padding: 0.5rem 1rem;
  border-bottom: 1px solid #1a1f29;
  display: flex;
  gap: 1rem;
  
  &:hover {
    background-color: #1a1f29;
  }

  ${props => props.$highlight && `
    background-color: rgba(245, 158, 11, 0.1);
  `}
`;

const LogTimestamp = styled.span`
  color: #6b7280;
  flex-shrink: 0;
`;

const LogLevel = styled.span`
  padding: 0.125rem 0.5rem;
  border-radius: 3px;
  font-size: 0.75rem;
  font-weight: 500;
  flex-shrink: 0;
  text-transform: uppercase;
  
  ${props => {
    switch(props.$level) {
      case 'error':
        return `
          background-color: rgba(239, 68, 68, 0.2);
          color: #ef4444;
        `;
      case 'warn':
        return `
          background-color: rgba(245, 158, 11, 0.2);
          color: #f59e0b;
        `;
      case 'info':
        return `
          background-color: rgba(59, 130, 246, 0.2);
          color: #3b82f6;
        `;
      case 'debug':
        return `
          background-color: rgba(107, 114, 128, 0.2);
          color: #6b7280;
        `;
      default:
        return `
          background-color: rgba(107, 114, 128, 0.2);
          color: #9ca3af;
        `;
    }
  }}
`;

const LogProgram = styled.span`
  color: #10b981;
  flex-shrink: 0;
`;

const LogMessage = styled.span`
  color: #e5e7eb;
  flex: 1;
  word-break: break-word;
`;

const NoLogs = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100%;
  color: #6b7280;
`;

function LogViewer({ logs = [] }) {
  const containerRef = useRef(null);
  const [filter, setFilter] = useState('all');
  const [search, setSearch] = useState('');
  const [autoScroll, setAutoScroll] = useState(true);

  useEffect(() => {
    if (autoScroll && containerRef.current) {
      containerRef.current.scrollTop = containerRef.current.scrollHeight;
    }
  }, [logs, autoScroll]);

  const handleScroll = () => {
    if (containerRef.current) {
      const { scrollTop, scrollHeight, clientHeight } = containerRef.current;
      const isAtBottom = scrollHeight - scrollTop - clientHeight < 10;
      setAutoScroll(isAtBottom);
    }
  };

  const filteredLogs = logs.filter(log => {
    if (filter !== 'all' && log.level !== filter) return false;
    if (search && !log.message.toLowerCase().includes(search.toLowerCase())) return false;
    return true;
  });

  const handleDownload = () => {
    const logContent = filteredLogs.map(log => 
      `[${format(new Date(log.timestamp), 'yyyy-MM-dd HH:mm:ss')}] [${log.level.toUpperCase()}] [${log.program}] ${log.message}`
    ).join('\n');
    
    const blob = new Blob([logContent], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `logs-${format(new Date(), 'yyyyMMdd-HHmmss')}.txt`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };

  return (
    <Container>
      <LogHeader>
        <FilterGroup>
          <FiFilter size={16} color="#6b7280" />
          <FilterSelect value={filter} onChange={(e) => setFilter(e.target.value)}>
            <option value="all">All Levels</option>
            <option value="error">Error</option>
            <option value="warn">Warning</option>
            <option value="info">Info</option>
            <option value="debug">Debug</option>
          </FilterSelect>
          <SearchInput
            type="text"
            placeholder="Search logs..."
            value={search}
            onChange={(e) => setSearch(e.target.value)}
          />
        </FilterGroup>
        <ActionGroup>
          <ActionButton onClick={handleDownload}>
            <FiDownload size={14} />
            Download
          </ActionButton>
        </ActionGroup>
      </LogHeader>

      <LogContainer ref={containerRef} onScroll={handleScroll}>
        {filteredLogs.length === 0 ? (
          <NoLogs>No logs to display</NoLogs>
        ) : (
          filteredLogs.map((log, index) => (
            <LogEntry key={`${log.timestamp}-${index}`} $highlight={search && log.message.toLowerCase().includes(search.toLowerCase())}>
              <LogTimestamp>
                {format(new Date(log.timestamp), 'HH:mm:ss.SSS')}
              </LogTimestamp>
              <LogLevel $level={log.level}>
                {log.level}
              </LogLevel>
              <LogProgram>
                [{log.program || 'system'}]
              </LogProgram>
              <LogMessage>
                {log.message}
              </LogMessage>
            </LogEntry>
          ))
        )}
      </LogContainer>
    </Container>
  );
}

export default LogViewer;