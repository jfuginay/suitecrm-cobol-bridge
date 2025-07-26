import React, { useState, useEffect } from 'react';
import { Card, Table, Button, Space, Tag, Modal, Form, Input, message, Tabs, Typography } from 'antd';
import { PlayCircleOutlined, PlusOutlined, DeleteOutlined, CheckCircleOutlined, CloseCircleOutlined, CopyOutlined } from '@ant-design/icons';
import { executeRule, generateTestData } from '../utils/ruleEngine';
import CodeMirror from '@uiw/react-codemirror';
import { javascript } from '@codemirror/lang-javascript';

const { TabPane } = Tabs;
const { Title, Text } = Typography;

const TestGenerator = ({ rules }) => {
  const [testCases, setTestCases] = useState([]);
  const [testResults, setTestResults] = useState({});
  const [showAddModal, setShowAddModal] = useState(false);
  const [selectedRule, setSelectedRule] = useState(null);
  const [form] = Form.useForm();
  
  useEffect(() => {
    // Generate initial test cases for all rules
    if (rules && rules.length > 0) {
      const generatedTests = [];
      rules.forEach(rule => {
        const ruleTests = generateTestData(rule);
        ruleTests.forEach(test => {
          generatedTests.push({
            id: `test-${Date.now()}-${Math.random()}`,
            ruleId: rule.id,
            ruleName: `Rule ${rule.id}`,
            ...test
          });
        });
      });
      setTestCases(generatedTests);
    }
  }, [rules]);
  
  const runTest = (testCase) => {
    const rule = rules.find(r => r.id === testCase.ruleId);
    if (!rule) {
      message.error('Rule not found');
      return;
    }
    
    try {
      const result = executeRule(rule, testCase.data);
      setTestResults(prev => ({
        ...prev,
        [testCase.id]: {
          ...result,
          passed: validateTestResult(testCase, result),
          timestamp: new Date().toISOString()
        }
      }));
      message.success('Test executed successfully');
    } catch (error) {
      setTestResults(prev => ({
        ...prev,
        [testCase.id]: {
          error: error.message,
          passed: false,
          timestamp: new Date().toISOString()
        }
      }));
      message.error('Test execution failed: ' + error.message);
    }
  };
  
  const runAllTests = () => {
    testCases.forEach(testCase => {
      runTest(testCase);
    });
  };
  
  const validateTestResult = (testCase, result) => {
    if (testCase.expectedBranch) {
      return result.branch === testCase.expectedBranch;
    }
    if (testCase.expectedCase) {
      return result.matchedCase === testCase.expectedCase;
    }
    return result.result === true;
  };
  
  const addTestCase = (values) => {
    const newTestCase = {
      id: `test-${Date.now()}`,
      ruleId: selectedRule.id,
      ruleName: `Rule ${selectedRule.id}`,
      name: values.name,
      data: JSON.parse(values.testData),
      expectedBranch: values.expectedBranch,
      expectedCase: values.expectedCase
    };
    
    setTestCases([...testCases, newTestCase]);
    setShowAddModal(false);
    form.resetFields();
    message.success('Test case added successfully');
  };
  
  const deleteTestCase = (testId) => {
    setTestCases(testCases.filter(tc => tc.id !== testId));
    const newResults = { ...testResults };
    delete newResults[testId];
    setTestResults(newResults);
    message.success('Test case deleted');
  };
  
  const exportTestReport = () => {
    const report = {
      generatedAt: new Date().toISOString(),
      summary: {
        total: testCases.length,
        passed: Object.values(testResults).filter(r => r.passed).length,
        failed: Object.values(testResults).filter(r => !r.passed).length
      },
      testCases: testCases.map(tc => ({
        ...tc,
        result: testResults[tc.id] || { status: 'not run' }
      }))
    };
    
    const blob = new Blob([JSON.stringify(report, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `test-report-${Date.now()}.json`;
    a.click();
    message.success('Test report exported');
  };
  
  const generateTestCode = (testCase) => {
    const rule = rules.find(r => r.id === testCase.ruleId);
    if (!rule) return '';
    
    return `// Test: ${testCase.name}
// Rule: ${testCase.ruleName}

const testData = ${JSON.stringify(testCase.data, null, 2)};

// Execute rule
const result = executeRule(rule, testData);

// Assert expected outcome
${testCase.expectedBranch ? 
  `expect(result.branch).toBe('${testCase.expectedBranch}');` :
  testCase.expectedCase ?
  `expect(result.matchedCase).toBe('${testCase.expectedCase}');` :
  'expect(result.result).toBe(true);'}

console.log('Test passed:', result);`;
  };
  
  const columns = [
    {
      title: 'Test Name',
      dataIndex: 'name',
      key: 'name',
      width: 200
    },
    {
      title: 'Rule',
      dataIndex: 'ruleName',
      key: 'ruleName',
      width: 120
    },
    {
      title: 'Test Data',
      dataIndex: 'data',
      key: 'data',
      render: (data) => (
        <Text code style={{ fontSize: 12 }}>
          {JSON.stringify(data, null, 2).substring(0, 100)}...
        </Text>
      )
    },
    {
      title: 'Expected',
      key: 'expected',
      width: 150,
      render: (_, record) => (
        <Space direction="vertical" size={0}>
          {record.expectedBranch && (
            <Tag color="blue">Branch: {record.expectedBranch}</Tag>
          )}
          {record.expectedCase && (
            <Tag color="purple">Case: {record.expectedCase}</Tag>
          )}
        </Space>
      )
    },
    {
      title: 'Result',
      key: 'result',
      width: 120,
      render: (_, record) => {
        const result = testResults[record.id];
        if (!result) {
          return <Tag>Not Run</Tag>;
        }
        
        return (
          <Space direction="vertical" size={0}>
            <Tag
              icon={result.passed ? <CheckCircleOutlined /> : <CloseCircleOutlined />}
              color={result.passed ? 'success' : 'error'}
            >
              {result.passed ? 'Passed' : 'Failed'}
            </Tag>
            {result.branch && <Text type="secondary">Branch: {result.branch}</Text>}
            {result.matchedCase && <Text type="secondary">Case: {result.matchedCase}</Text>}
          </Space>
        );
      }
    },
    {
      title: 'Actions',
      key: 'actions',
      width: 150,
      render: (_, record) => (
        <Space>
          <Button
            size="small"
            icon={<PlayCircleOutlined />}
            onClick={() => runTest(record)}
          >
            Run
          </Button>
          <Button
            size="small"
            danger
            icon={<DeleteOutlined />}
            onClick={() => deleteTestCase(record.id)}
          />
        </Space>
      )
    }
  ];
  
  return (
    <Card
      title="Test Case Generator"
      className="test-generator"
      extra={
        <Space>
          <Button icon={<PlayCircleOutlined />} onClick={runAllTests}>
            Run All Tests
          </Button>
          <Button icon={<PlusOutlined />} onClick={() => {
            if (rules.length > 0) {
              setSelectedRule(rules[0]);
              setShowAddModal(true);
            } else {
              message.warning('No rules available to test');
            }
          }}>
            Add Test Case
          </Button>
          <Button onClick={exportTestReport}>
            Export Report
          </Button>
        </Space>
      }
    >
      <Tabs defaultActiveKey="tests">
        <TabPane tab="Test Cases" key="tests">
          <Table
            columns={columns}
            dataSource={testCases}
            rowKey="id"
            size="small"
            pagination={{ pageSize: 10 }}
          />
          
          {testCases.length > 0 && (
            <div style={{ marginTop: 16 }}>
              <Title level={5}>Test Summary</Title>
              <Space>
                <Tag>Total: {testCases.length}</Tag>
                <Tag color="success">
                  Passed: {Object.values(testResults).filter(r => r.passed).length}
                </Tag>
                <Tag color="error">
                  Failed: {Object.values(testResults).filter(r => !r.passed).length}
                </Tag>
                <Tag>
                  Not Run: {testCases.length - Object.keys(testResults).length}
                </Tag>
              </Space>
            </div>
          )}
        </TabPane>
        
        <TabPane tab="Test Code" key="code">
          {testCases.map(testCase => (
            <Card
              key={testCase.id}
              size="small"
              title={testCase.name}
              extra={
                <Button
                  size="small"
                  icon={<CopyOutlined />}
                  onClick={() => {
                    navigator.clipboard.writeText(generateTestCode(testCase));
                    message.success('Code copied to clipboard');
                  }}
                >
                  Copy
                </Button>
              }
              style={{ marginBottom: 16 }}
            >
              <CodeMirror
                value={generateTestCode(testCase)}
                height="200px"
                theme="light"
                extensions={[javascript()]}
                editable={false}
              />
            </Card>
          ))}
        </TabPane>
      </Tabs>
      
      <Modal
        title="Add Test Case"
        visible={showAddModal}
        onCancel={() => setShowAddModal(false)}
        footer={null}
        width={600}
      >
        <Form
          form={form}
          layout="vertical"
          onFinish={addTestCase}
          initialValues={{
            testData: JSON.stringify({
              "FIELD-NAME": "value"
            }, null, 2)
          }}
        >
          <Form.Item
            name="name"
            label="Test Name"
            rules={[{ required: true, message: 'Please enter test name' }]}
          >
            <Input placeholder="e.g., Premium customer high value order" />
          </Form.Item>
          
          <Form.Item
            name="testData"
            label="Test Data (JSON)"
            rules={[
              { required: true, message: 'Please enter test data' },
              {
                validator: (_, value) => {
                  try {
                    JSON.parse(value);
                    return Promise.resolve();
                  } catch (e) {
                    return Promise.reject(new Error('Invalid JSON format'));
                  }
                }
              }
            ]}
          >
            <TextArea rows={6} style={{ fontFamily: 'monospace' }} />
          </Form.Item>
          
          {selectedRule && selectedRule.type === 'decision' && (
            <Form.Item name="expectedBranch" label="Expected Branch">
              <Select placeholder="Select expected branch">
                <Select.Option value="true">True</Select.Option>
                <Select.Option value="false">False</Select.Option>
              </Select>
            </Form.Item>
          )}
          
          {selectedRule && selectedRule.type === 'evaluate' && (
            <Form.Item name="expectedCase" label="Expected Case">
              <Input placeholder="e.g., WHEN 750 THRU 850" />
            </Form.Item>
          )}
          
          <Form.Item>
            <Space>
              <Button type="primary" htmlType="submit">
                Add Test Case
              </Button>
              <Button onClick={() => setShowAddModal(false)}>
                Cancel
              </Button>
            </Space>
          </Form.Item>
        </Form>
      </Modal>
    </Card>
  );
};

export default TestGenerator;