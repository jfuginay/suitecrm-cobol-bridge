import React from 'react';
import { Handle, Position } from 'react-flow-renderer';
import { Tag } from 'antd';
import {
  BranchesOutlined,
  PlayCircleOutlined,
  QuestionCircleOutlined,
  ClusterOutlined
} from '@ant-design/icons';

const CustomNode = ({ data, selected }) => {
  const getNodeStyle = () => {
    const baseStyle = {
      padding: '10px 15px',
      borderRadius: '8px',
      border: '2px solid',
      minWidth: '150px',
      textAlign: 'center',
      background: '#fff',
      fontSize: '14px',
      fontFamily: 'monospace'
    };
    
    switch (data.type) {
      case 'decision':
        return {
          ...baseStyle,
          borderColor: '#91d5ff',
          background: '#e6f7ff'
        };
      case 'action':
        return {
          ...baseStyle,
          borderColor: '#b7eb8f',
          background: '#f6ffed'
        };
      case 'condition':
        return {
          ...baseStyle,
          borderColor: '#ffd591',
          background: '#fff7e6'
        };
      case 'evaluate':
        return {
          ...baseStyle,
          borderColor: '#d3adf7',
          background: '#f9f0ff'
        };
      default:
        return baseStyle;
    }
  };
  
  const getIcon = () => {
    switch (data.type) {
      case 'decision':
        return <BranchesOutlined />;
      case 'action':
        return <PlayCircleOutlined />;
      case 'condition':
        return <QuestionCircleOutlined />;
      case 'evaluate':
        return <ClusterOutlined />;
      default:
        return null;
    }
  };
  
  const getTypeTag = () => {
    const colors = {
      decision: 'blue',
      action: 'green',
      condition: 'orange',
      evaluate: 'purple'
    };
    
    return (
      <Tag color={colors[data.type] || 'default'} style={{ margin: 0 }}>
        {data.type?.toUpperCase()}
      </Tag>
    );
  };
  
  return (
    <div style={getNodeStyle()} className={selected ? 'selected' : ''}>
      <Handle
        type="target"
        position={Position.Top}
        style={{ background: '#555' }}
      />
      
      <div style={{ marginBottom: 5 }}>
        {getTypeTag()}
      </div>
      
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, justifyContent: 'center' }}>
        {getIcon()}
        <div style={{ flex: 1, textAlign: 'left' }}>{data.label}</div>
      </div>
      
      <Handle
        type="source"
        position={Position.Bottom}
        style={{ background: '#555' }}
      />
    </div>
  );
};

export default CustomNode;