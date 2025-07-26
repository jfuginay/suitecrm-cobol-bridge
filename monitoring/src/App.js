import React, { useEffect, useState } from 'react';
import styled from 'styled-components';
import { ToastContainer } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';
import Dashboard from './components/Dashboard';
import websocket from './services/websocket';

const AppContainer = styled.div`
  min-height: 100vh;
  background-color: #0f1419;
  color: #e1e8ed;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
    'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
    sans-serif;
`;

const Header = styled.header`
  background-color: #1a1f29;
  border-bottom: 1px solid #2f3741;
  padding: 1rem 2rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const Title = styled.h1`
  margin: 0;
  font-size: 1.5rem;
  font-weight: 600;
  color: #fff;
  display: flex;
  align-items: center;
  gap: 0.5rem;
`;

const ConnectionStatus = styled.div`
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.875rem;
  color: ${props => props.$connected ? '#10b981' : '#ef4444'};
`;

const StatusDot = styled.div`
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background-color: ${props => props.$connected ? '#10b981' : '#ef4444'};
  animation: ${props => props.$connected ? 'pulse 2s infinite' : 'none'};

  @keyframes pulse {
    0% {
      box-shadow: 0 0 0 0 rgba(16, 185, 129, 0.4);
    }
    70% {
      box-shadow: 0 0 0 10px rgba(16, 185, 129, 0);
    }
    100% {
      box-shadow: 0 0 0 0 rgba(16, 185, 129, 0);
    }
  }
`;

function App() {
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    // Connect to WebSocket
    websocket.connect();

    // Listen for connection status
    const handleConnectionStatus = ({ connected }) => {
      setIsConnected(connected);
    };

    websocket.on('connection:status', handleConnectionStatus);

    return () => {
      websocket.off('connection:status', handleConnectionStatus);
    };
  }, []);

  return (
    <AppContainer>
      <Header>
        <Title>
          SuiteCRM-COBOL Bridge Monitor
        </Title>
        <ConnectionStatus $connected={isConnected}>
          <StatusDot $connected={isConnected} />
          {isConnected ? 'Connected' : 'Disconnected'}
        </ConnectionStatus>
      </Header>
      <Dashboard />
      <ToastContainer
        position="bottom-right"
        autoClose={5000}
        hideProgressBar={false}
        newestOnTop
        closeOnClick
        rtl={false}
        pauseOnFocusLoss
        draggable
        pauseOnHover
        theme="dark"
      />
    </AppContainer>
  );
}

export default App;