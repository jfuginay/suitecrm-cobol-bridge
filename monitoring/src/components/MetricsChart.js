import React, { useEffect, useState } from 'react';
import styled from 'styled-components';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
  Filler
} from 'chart.js';
import { Line } from 'react-chartjs-2';

ChartJS.register(
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
  Filler
);

const Container = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 1rem;
`;

const ChartWrapper = styled.div`
  flex: 1;
  position: relative;
  min-height: 300px;
`;

const MetricSelector = styled.div`
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
`;

const MetricButton = styled.button`
  padding: 0.5rem 1rem;
  border-radius: 6px;
  border: 1px solid ${props => props.$active ? '#10b981' : '#2f3741'};
  background-color: ${props => props.$active ? 'rgba(16, 185, 129, 0.1)' : 'transparent'};
  color: ${props => props.$active ? '#10b981' : '#9ca3af'};
  font-size: 0.875rem;
  cursor: pointer;
  transition: all 0.2s ease;

  &:hover {
    border-color: #10b981;
    color: #10b981;
  }
`;

const NoData = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100%;
  color: #6b7280;
`;

function MetricsChart({ metrics }) {
  const [selectedMetric, setSelectedMetric] = useState('cpu');
  const [chartData, setChartData] = useState(null);

  const metricTypes = [
    { id: 'cpu', label: 'CPU Usage', unit: '%', color: '#10b981' },
    { id: 'memory', label: 'Memory', unit: 'MB', color: '#3b82f6' },
    { id: 'throughput', label: 'Throughput', unit: 'req/s', color: '#f59e0b' },
    { id: 'response', label: 'Response Time', unit: 'ms', color: '#ef4444' }
  ];

  useEffect(() => {
    if (!metrics || !metrics.data) return;

    const metric = metricTypes.find(m => m.id === selectedMetric);
    const data = metrics.data[selectedMetric] || [];

    setChartData({
      labels: data.map(d => {
        const date = new Date(d.timestamp);
        return date.toLocaleTimeString('en-US', { 
          hour: '2-digit', 
          minute: '2-digit' 
        });
      }),
      datasets: [
        {
          label: metric.label,
          data: data.map(d => d.value),
          borderColor: metric.color,
          backgroundColor: `${metric.color}20`,
          borderWidth: 2,
          fill: true,
          tension: 0.4,
          pointRadius: 0,
          pointHoverRadius: 4,
          pointBackgroundColor: metric.color,
          pointBorderColor: '#0f1419',
          pointBorderWidth: 2
        }
      ]
    });
  }, [metrics, selectedMetric]);

  const options = {
    responsive: true,
    maintainAspectRatio: false,
    plugins: {
      legend: {
        display: false
      },
      tooltip: {
        mode: 'index',
        intersect: false,
        backgroundColor: '#1a1f29',
        titleColor: '#fff',
        bodyColor: '#e5e7eb',
        borderColor: '#2f3741',
        borderWidth: 1,
        padding: 12,
        displayColors: false,
        callbacks: {
          label: (context) => {
            const metric = metricTypes.find(m => m.id === selectedMetric);
            return `${context.parsed.y} ${metric.unit}`;
          }
        }
      }
    },
    scales: {
      x: {
        grid: {
          color: '#2f3741',
          drawBorder: false
        },
        ticks: {
          color: '#6b7280',
          maxRotation: 0,
          autoSkipPadding: 20
        }
      },
      y: {
        grid: {
          color: '#2f3741',
          drawBorder: false
        },
        ticks: {
          color: '#6b7280',
          callback: function(value) {
            const metric = metricTypes.find(m => m.id === selectedMetric);
            return `${value} ${metric.unit}`;
          }
        }
      }
    },
    interaction: {
      mode: 'nearest',
      axis: 'x',
      intersect: false
    }
  };

  if (!metrics || !chartData) {
    return (
      <Container>
        <NoData>No metrics data available</NoData>
      </Container>
    );
  }

  return (
    <Container>
      <MetricSelector>
        {metricTypes.map(metric => (
          <MetricButton
            key={metric.id}
            $active={selectedMetric === metric.id}
            onClick={() => setSelectedMetric(metric.id)}
          >
            {metric.label}
          </MetricButton>
        ))}
      </MetricSelector>
      <ChartWrapper>
        <Line data={chartData} options={options} />
      </ChartWrapper>
    </Container>
  );
}

export default MetricsChart;