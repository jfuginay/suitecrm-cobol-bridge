import React, { useState, useEffect } from 'react';
import { Card, Form, Input, Select, Button, Space, Divider, Tag, Modal, message } from 'antd';
import { PlusOutlined, DeleteOutlined, EditOutlined, SaveOutlined } from '@ant-design/icons';
import { DndProvider } from 'react-dnd';
import { HTML5Backend } from 'react-dnd-html5-backend';

const { Option } = Select;
const { TextArea } = Input;

const RuleEditor = ({ selectedNode, onRuleUpdate, onRuleAdd, onRuleDelete }) => {
  const [form] = Form.useForm();
  const [editMode, setEditMode] = useState(false);
  const [showAddModal, setShowAddModal] = useState(false);
  
  useEffect(() => {
    if (selectedNode && selectedNode.data) {
      const { rule, action, case: caseData } = selectedNode.data;
      
      if (rule) {
        form.setFieldsValue({
          type: rule.type,
          condition: formatConditionForEdit(rule.condition),
          subject: rule.subject,
          cases: rule.cases
        });
      } else if (action) {
        form.setFieldsValue({
          actionType: action.type,
          ...action
        });
      } else if (caseData) {
        form.setFieldsValue({
          caseCondition: caseData.condition,
          caseActions: caseData.actions
        });
      }
    }
  }, [selectedNode, form]);
  
  const formatConditionForEdit = (condition) => {
    if (!condition) return '';
    
    if (condition.type === 'comparison') {
      return `${condition.field} ${condition.operator} ${condition.value}`;
    } else if (condition.type === 'compound') {
      return condition.conditions
        .map(c => formatConditionForEdit(c))
        .join(` ${condition.operator} `);
    } else if (condition.type === 'function') {
      return `${condition.function}(${condition.argument})`;
    } else if (condition.type === 'boolean') {
      return condition.field;
    }
    
    return JSON.stringify(condition);
  };
  
  const handleSave = () => {
    form.validateFields().then(values => {
      if (onRuleUpdate) {
        onRuleUpdate(selectedNode.id, values);
        setEditMode(false);
        message.success('Rule updated successfully');
      }
    });
  };
  
  const handleAddRule = (values) => {
    if (onRuleAdd) {
      onRuleAdd(values);
      setShowAddModal(false);
      form.resetFields();
      message.success('New rule added successfully');
    }
  };
  
  const handleDelete = () => {
    Modal.confirm({
      title: 'Delete Rule',
      content: 'Are you sure you want to delete this rule?',
      okText: 'Delete',
      okType: 'danger',
      onOk: () => {
        if (onRuleDelete && selectedNode) {
          onRuleDelete(selectedNode.id);
          message.success('Rule deleted successfully');
        }
      }
    });
  };
  
  const renderConditionBuilder = () => (
    <Space direction="vertical" style={{ width: '100%' }}>
      <Form.Item name="field" label="Field">
        <Input placeholder="e.g., CUSTOMER-TYPE" />
      </Form.Item>
      <Form.Item name="operator" label="Operator">
        <Select placeholder="Select operator">
          <Option value="=">=</Option>
          <Option value="!=">!=</Option>
          <Option value=">">></Option>
          <Option value="<"><</Option>
          <Option value=">=">>=</Option>
          <Option value="<="><=</Option>
        </Select>
      </Form.Item>
      <Form.Item name="value" label="Value">
        <Input placeholder="e.g., PREMIUM" />
      </Form.Item>
    </Space>
  );
  
  const renderActionBuilder = () => (
    <Space direction="vertical" style={{ width: '100%' }}>
      <Form.Item name="actionType" label="Action Type">
        <Select placeholder="Select action type">
          <Option value="assignment">Assignment (MOVE)</Option>
          <Option value="computation">Computation (COMPUTE)</Option>
          <Option value="perform">Perform Procedure</Option>
          <Option value="display">Display Message</Option>
        </Select>
      </Form.Item>
      
      <Form.Item
        noStyle
        shouldUpdate={(prevValues, currentValues) => 
          prevValues.actionType !== currentValues.actionType
        }
      >
        {({ getFieldValue }) => {
          const actionType = getFieldValue('actionType');
          
          switch (actionType) {
            case 'assignment':
              return (
                <>
                  <Form.Item name="source" label="Source">
                    <Input placeholder="Value or field to move" />
                  </Form.Item>
                  <Form.Item name="target" label="Target">
                    <Input placeholder="Target field" />
                  </Form.Item>
                </>
              );
              
            case 'computation':
              return (
                <>
                  <Form.Item name="target" label="Target">
                    <Input placeholder="Target field" />
                  </Form.Item>
                  <Form.Item name="expression" label="Expression">
                    <Input placeholder="e.g., AMOUNT * 0.15" />
                  </Form.Item>
                </>
              );
              
            case 'perform':
              return (
                <Form.Item name="procedure" label="Procedure">
                  <Input placeholder="Procedure name" />
                </Form.Item>
              );
              
            case 'display':
              return (
                <Form.Item name="message" label="Message">
                  <TextArea placeholder="Message to display" />
                </Form.Item>
              );
              
            default:
              return null;
          }
        }}
      </Form.Item>
    </Space>
  );
  
  if (!selectedNode) {
    return (
      <Card title="Rule Editor" className="rule-editor">
        <div style={{ textAlign: 'center', padding: '40px 0' }}>
          <p style={{ marginBottom: 20 }}>Select a node from the decision tree to edit</p>
          <Button
            type="primary"
            icon={<PlusOutlined />}
            onClick={() => setShowAddModal(true)}
          >
            Add New Rule
          </Button>
        </div>
        
        <Modal
          title="Add New Rule"
          visible={showAddModal}
          onCancel={() => setShowAddModal(false)}
          footer={null}
          width={600}
        >
          <Form
            layout="vertical"
            onFinish={handleAddRule}
          >
            <Form.Item name="type" label="Rule Type" rules={[{ required: true }]}>
              <Select placeholder="Select rule type">
                <Option value="decision">Decision (IF)</Option>
                <Option value="evaluate">Evaluate (SWITCH)</Option>
              </Select>
            </Form.Item>
            
            <Form.Item
              noStyle
              shouldUpdate={(prevValues, currentValues) => 
                prevValues.type !== currentValues.type
              }
            >
              {({ getFieldValue }) => {
                const type = getFieldValue('type');
                
                if (type === 'decision') {
                  return renderConditionBuilder();
                } else if (type === 'evaluate') {
                  return (
                    <Form.Item name="subject" label="Subject" rules={[{ required: true }]}>
                      <Input placeholder="e.g., CREDIT-SCORE" />
                    </Form.Item>
                  );
                }
                
                return null;
              }}
            </Form.Item>
            
            <Form.Item>
              <Space>
                <Button type="primary" htmlType="submit">
                  Add Rule
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
  }
  
  return (
    <DndProvider backend={HTML5Backend}>
      <Card
        title={
          <Space>
            <span>Rule Editor</span>
            <Tag color={selectedNode.data.type === 'decision' ? 'blue' : 'green'}>
              {selectedNode.data.type?.toUpperCase()}
            </Tag>
          </Space>
        }
        extra={
          <Space>
            {!editMode ? (
              <Button icon={<EditOutlined />} onClick={() => setEditMode(true)}>
                Edit
              </Button>
            ) : (
              <>
                <Button type="primary" icon={<SaveOutlined />} onClick={handleSave}>
                  Save
                </Button>
                <Button onClick={() => setEditMode(false)}>
                  Cancel
                </Button>
              </>
            )}
            <Button danger icon={<DeleteOutlined />} onClick={handleDelete}>
              Delete
            </Button>
          </Space>
        }
        className="rule-editor"
      >
        <Form
          form={form}
          layout="vertical"
          disabled={!editMode}
        >
          {selectedNode.data.type === 'decision' && (
            <>
              <Form.Item label="Condition" name="condition">
                <TextArea rows={3} placeholder="Enter condition" />
              </Form.Item>
              
              <Divider>Condition Builder</Divider>
              {renderConditionBuilder()}
              
              <Divider>Actions</Divider>
              <Form.Item label="True Branch Actions">
                <Button icon={<PlusOutlined />} size="small">
                  Add Action
                </Button>
              </Form.Item>
              
              <Form.Item label="False Branch Actions">
                <Button icon={<PlusOutlined />} size="small">
                  Add Action
                </Button>
              </Form.Item>
            </>
          )}
          
          {selectedNode.data.type === 'action' && (
            <>
              <Divider>Action Details</Divider>
              {renderActionBuilder()}
            </>
          )}
          
          {selectedNode.data.type === 'evaluate' && (
            <>
              <Form.Item label="Subject" name="subject">
                <Input placeholder="e.g., CREDIT-SCORE" />
              </Form.Item>
              
              <Divider>Cases</Divider>
              <Form.Item label="Cases">
                <Button icon={<PlusOutlined />} size="small">
                  Add Case
                </Button>
              </Form.Item>
            </>
          )}
        </Form>
      </Card>
    </DndProvider>
  );
};

export default RuleEditor;