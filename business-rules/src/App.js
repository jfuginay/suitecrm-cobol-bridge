import React, { useState } from 'react';
import { Layout, Menu, Typography, Space, Badge, Button, Divider } from 'antd';
import {
  CodeOutlined,
  PartitionOutlined,
  EditOutlined,
  ExperimentOutlined,
  ExportOutlined,
  GithubOutlined,
  QuestionCircleOutlined
} from '@ant-design/icons';

import RuleExtractor from './components/RuleExtractor';
import DecisionTree from './components/DecisionTree';
import RuleEditor from './components/RuleEditor';
import TestGenerator from './components/TestGenerator';
import CobolExporter from './components/CobolExporter';

import { rulesToFlowElements } from './utils/cobolParser';
import './App.css';

const { Header, Content, Sider } = Layout;
const { Title, Text } = Typography;

function App() {
  const [collapsed, setCollapsed] = useState(false);
  const [activeTab, setActiveTab] = useState('extract');
  const [extractedRules, setExtractedRules] = useState([]);
  const [flowElements, setFlowElements] = useState({ nodes: [], edges: [] });
  const [selectedNode, setSelectedNode] = useState(null);
  
  const handleRulesExtracted = (rules) => {
    setExtractedRules(rules);
    const elements = rulesToFlowElements(rules);
    setFlowElements(elements);
    setActiveTab('visualize');
  };
  
  const handleNodeSelect = (node) => {
    setSelectedNode(node);
    if (activeTab !== 'edit') {
      setActiveTab('edit');
    }
  };
  
  const handleRuleUpdate = (nodeId, updatedData) => {
    // Update the rule in extractedRules
    const updatedRules = extractedRules.map(rule => {
      if (rule.id === nodeId) {
        return { ...rule, ...updatedData };
      }
      return rule;
    });
    
    setExtractedRules(updatedRules);
    
    // Regenerate flow elements
    const elements = rulesToFlowElements(updatedRules);
    setFlowElements(elements);
  };
  
  const handleRuleAdd = (newRule) => {
    const rule = {
      id: `rule-${Date.now()}`,
      ...newRule,
      trueBranch: [],
      falseBranch: [],
      cases: newRule.type === 'evaluate' ? [] : undefined
    };
    
    const updatedRules = [...extractedRules, rule];
    setExtractedRules(updatedRules);
    
    const elements = rulesToFlowElements(updatedRules);
    setFlowElements(elements);
  };
  
  const handleRuleDelete = (nodeId) => {
    const updatedRules = extractedRules.filter(rule => rule.id !== nodeId);
    setExtractedRules(updatedRules);
    
    const elements = rulesToFlowElements(updatedRules);
    setFlowElements(elements);
    
    setSelectedNode(null);
  };
  
  const menuItems = [
    {
      key: 'extract',
      icon: <CodeOutlined />,
      label: 'Extract Rules',
      description: 'Import COBOL code'
    },
    {
      key: 'visualize',
      icon: <PartitionOutlined />,
      label: (
        <Space>
          <span>Visualize</span>
          {extractedRules.length > 0 && (
            <Badge count={extractedRules.length} style={{ backgroundColor: '#52c41a' }} />
          )}
        </Space>
      ),
      description: 'View decision trees'
    },
    {
      key: 'edit',
      icon: <EditOutlined />,
      label: 'Edit Rules',
      description: 'Modify business logic'
    },
    {
      key: 'test',
      icon: <ExperimentOutlined />,
      label: 'Test Cases',
      description: 'Generate & run tests'
    },
    {
      key: 'export',
      icon: <ExportOutlined />,
      label: 'Export',
      description: 'Generate COBOL code'
    }
  ];
  
  const renderContent = () => {
    switch (activeTab) {
      case 'extract':
        return <RuleExtractor onRulesExtracted={handleRulesExtracted} />;
        
      case 'visualize':
        return (
          <DecisionTree
            elements={flowElements}
            onNodeSelect={handleNodeSelect}
          />
        );
        
      case 'edit':
        return (
          <RuleEditor
            selectedNode={selectedNode}
            onRuleUpdate={handleRuleUpdate}
            onRuleAdd={handleRuleAdd}
            onRuleDelete={handleRuleDelete}
          />
        );
        
      case 'test':
        return <TestGenerator rules={extractedRules} />;
        
      case 'export':
        return <CobolExporter rules={extractedRules} flowElements={flowElements} />;
        
      default:
        return null;
    }
  };
  
  return (
    <Layout style={{ minHeight: '100vh' }}>
      <Header style={{ background: '#001529', padding: '0 24px' }}>
        <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
          <Space>
            <PartitionOutlined style={{ fontSize: 24, color: '#1890ff' }} />
            <Title level={3} style={{ color: '#fff', margin: 0 }}>
              COBOL Business Rules Editor
            </Title>
          </Space>
          <Space>
            <Button type="text" icon={<GithubOutlined />} style={{ color: '#fff' }}>
              GitHub
            </Button>
            <Button type="text" icon={<QuestionCircleOutlined />} style={{ color: '#fff' }}>
              Help
            </Button>
          </Space>
        </div>
      </Header>
      
      <Layout>
        <Sider
          collapsible
          collapsed={collapsed}
          onCollapse={setCollapsed}
          width={250}
          style={{ background: '#fff' }}
        >
          <Menu
            mode="inline"
            selectedKeys={[activeTab]}
            onClick={({ key }) => setActiveTab(key)}
            style={{ height: '100%', borderRight: 0 }}
          >
            {menuItems.map(item => (
              <Menu.Item key={item.key} icon={item.icon}>
                {!collapsed ? (
                  <div>
                    <div>{item.label}</div>
                    <Text type="secondary" style={{ fontSize: 12 }}>
                      {item.description}
                    </Text>
                  </div>
                ) : (
                  item.label
                )}
              </Menu.Item>
            ))}
          </Menu>
        </Sider>
        
        <Layout style={{ padding: '24px' }}>
          <Content
            style={{
              background: '#f0f2f5',
              padding: 0,
              margin: 0,
              minHeight: 280,
            }}
          >
            {renderContent()}
          </Content>
        </Layout>
      </Layout>
    </Layout>
  );
}

export default App;