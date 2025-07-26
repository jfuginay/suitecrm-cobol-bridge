import React, { useCallback, useEffect } from 'react';
import ReactFlow, {
  MiniMap,
  Controls,
  Background,
  useNodesState,
  useEdgesState,
  addEdge,
  MarkerType
} from 'react-flow-renderer';
import { Card, Button, Space } from 'antd';
import { ZoomInOutlined, ZoomOutOutlined, FullscreenOutlined } from '@ant-design/icons';
import dagre from 'dagre';
import CustomNode from './CustomNode';

const nodeTypes = {
  custom: CustomNode
};

const DecisionTree = ({ elements, onNodeSelect, onEdgeUpdate }) => {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  
  useEffect(() => {
    if (elements && elements.nodes && elements.edges) {
      // Apply automatic layout
      const layoutedElements = getLayoutedElements(elements.nodes, elements.edges);
      setNodes(layoutedElements.nodes);
      setEdges(layoutedElements.edges);
    }
  }, [elements, setNodes, setEdges]);
  
  const onConnect = useCallback(
    (params) => {
      const newEdge = {
        ...params,
        type: 'smoothstep',
        animated: true,
        markerEnd: {
          type: MarkerType.ArrowClosed
        }
      };
      setEdges((eds) => addEdge(newEdge, eds));
      if (onEdgeUpdate) {
        onEdgeUpdate('add', newEdge);
      }
    },
    [setEdges, onEdgeUpdate]
  );
  
  const onNodeClick = useCallback(
    (event, node) => {
      if (onNodeSelect) {
        onNodeSelect(node);
      }
    },
    [onNodeSelect]
  );
  
  const onEdgeClick = useCallback(
    (event, edge) => {
      // Handle edge click for editing
      console.log('Edge clicked:', edge);
    },
    []
  );
  
  const onNodeDragStop = useCallback(
    (event, node) => {
      // Update node position after drag
      console.log('Node dragged:', node);
    },
    []
  );
  
  const fitView = () => {
    const reactFlowInstance = document.querySelector('.react-flow__renderer');
    if (reactFlowInstance) {
      reactFlowInstance.fitView({ padding: 0.2 });
    }
  };
  
  return (
    <Card
      title="Visual Decision Tree"
      className="decision-tree"
      extra={
        <Space>
          <Button icon={<ZoomInOutlined />} size="small" />
          <Button icon={<ZoomOutOutlined />} size="small" />
          <Button icon={<FullscreenOutlined />} size="small" onClick={fitView} />
        </Space>
      }
    >
      <div style={{ height: 600 }}>
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onConnect={onConnect}
          onNodeClick={onNodeClick}
          onEdgeClick={onEdgeClick}
          onNodeDragStop={onNodeDragStop}
          nodeTypes={nodeTypes}
          fitView
          attributionPosition="bottom-left"
        >
          <Controls />
          <MiniMap
            nodeColor={(node) => {
              switch (node.data?.type) {
                case 'decision':
                  return '#e6f7ff';
                case 'action':
                  return '#f6ffed';
                case 'condition':
                  return '#fff7e6';
                case 'evaluate':
                  return '#f9f0ff';
                default:
                  return '#fff';
              }
            }}
            nodeStrokeWidth={3}
            pannable
            zoomable
          />
          <Background variant="dots" gap={12} size={1} />
        </ReactFlow>
      </div>
    </Card>
  );
};

// Automatic layout using dagre
const getLayoutedElements = (nodes, edges, direction = 'TB') => {
  const dagreGraph = new dagre.graphlib.Graph();
  dagreGraph.setDefaultEdgeLabel(() => ({}));
  
  const nodeWidth = 200;
  const nodeHeight = 80;
  
  dagreGraph.setGraph({ rankdir: direction, nodesep: 100, ranksep: 100 });
  
  nodes.forEach((node) => {
    dagreGraph.setNode(node.id, { width: nodeWidth, height: nodeHeight });
  });
  
  edges.forEach((edge) => {
    dagreGraph.setEdge(edge.source, edge.target);
  });
  
  dagre.layout(dagreGraph);
  
  const layoutedNodes = nodes.map((node) => {
    const nodeWithPosition = dagreGraph.node(node.id);
    
    return {
      ...node,
      position: {
        x: nodeWithPosition.x - nodeWidth / 2,
        y: nodeWithPosition.y - nodeHeight / 2
      }
    };
  });
  
  return { nodes: layoutedNodes, edges };
};

export default DecisionTree;