import React, { useState } from 'react';
import { Card, Button, Upload, message, Tabs, Typography } from 'antd';
import { UploadOutlined, CodeOutlined, FileTextOutlined } from '@ant-design/icons';
import CodeMirror from '@uiw/react-codemirror';
import { extractBusinessRules } from '../utils/cobolParser';
import sampleRules from '../data/sampleRules.json';

const { TabPane } = Tabs;
const { Title, Paragraph } = Typography;

const RuleExtractor = ({ onRulesExtracted }) => {
  const [cobolCode, setCobolCode] = useState('');
  const [extractedRules, setExtractedRules] = useState([]);
  
  const handleExtract = () => {
    if (!cobolCode.trim()) {
      message.warning('Please enter COBOL code to extract rules');
      return;
    }
    
    try {
      const rules = extractBusinessRules(cobolCode);
      setExtractedRules(rules);
      onRulesExtracted(rules);
      message.success(`Extracted ${rules.length} business rules`);
    } catch (error) {
      message.error('Failed to extract rules: ' + error.message);
    }
  };
  
  const handleFileUpload = (file) => {
    const reader = new FileReader();
    reader.onload = (e) => {
      setCobolCode(e.target.result);
      message.success('File loaded successfully');
    };
    reader.readAsText(file);
    return false; // Prevent default upload
  };
  
  const loadSampleRule = (ruleId) => {
    const rule = sampleRules.rules.find(r => r.id === ruleId);
    if (rule) {
      setCobolCode(rule.cobolCode);
      message.success(`Loaded sample: ${rule.name}`);
    }
  };
  
  return (
    <Card title="Business Rule Extractor" className="rule-extractor">
      <Tabs defaultActiveKey="manual">
        <TabPane tab={<span><CodeOutlined /> Manual Input</span>} key="manual">
          <div style={{ marginBottom: 16 }}>
            <Title level={5}>Enter COBOL Code</Title>
            <Paragraph type="secondary">
              Paste your COBOL procedure division code containing business rules
            </Paragraph>
          </div>
          
          <CodeMirror
            value={cobolCode}
            height="400px"
            theme="light"
            onChange={(value) => setCobolCode(value)}
            placeholder="       IF CUSTOMER-TYPE = 'PREMIUM'
          COMPUTE DISCOUNT = ORDER-AMOUNT * 0.15
       ELSE
          COMPUTE DISCOUNT = ORDER-AMOUNT * 0.05
       END-IF"
          />
          
          <div style={{ marginTop: 16, display: 'flex', gap: 8 }}>
            <Button type="primary" onClick={handleExtract} icon={<CodeOutlined />}>
              Extract Rules
            </Button>
            <Button onClick={() => setCobolCode('')}>
              Clear
            </Button>
          </div>
        </TabPane>
        
        <TabPane tab={<span><UploadOutlined /> Upload File</span>} key="upload">
          <Upload
            beforeUpload={handleFileUpload}
            accept=".cob,.cbl,.cobol,.txt"
            showUploadList={false}
          >
            <Button icon={<UploadOutlined />}>
              Click to Upload COBOL File
            </Button>
          </Upload>
          <Paragraph type="secondary" style={{ marginTop: 8 }}>
            Supported formats: .cob, .cbl, .cobol, .txt
          </Paragraph>
        </TabPane>
        
        <TabPane tab={<span><FileTextOutlined /> Sample Rules</span>} key="samples">
          <Title level={5}>Load Sample Business Rules</Title>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
            {sampleRules.rules.map(rule => (
              <Card
                key={rule.id}
                size="small"
                hoverable
                onClick={() => loadSampleRule(rule.id)}
                style={{ cursor: 'pointer' }}
              >
                <Title level={5} style={{ margin: 0 }}>{rule.name}</Title>
                <Paragraph type="secondary" style={{ margin: 0 }}>
                  {rule.description}
                </Paragraph>
              </Card>
            ))}
          </div>
        </TabPane>
      </Tabs>
      
      {extractedRules.length > 0 && (
        <div style={{ marginTop: 24 }}>
          <Title level={5}>Extraction Summary</Title>
          <Paragraph>
            Found {extractedRules.length} business rule{extractedRules.length !== 1 ? 's' : ''}:
          </Paragraph>
          <ul>
            {extractedRules.map((rule, index) => (
              <li key={index}>
                {rule.type === 'decision' ? 'IF' : 'EVALUATE'} statement at line {rule.lineNumber}
              </li>
            ))}
          </ul>
        </div>
      )}
    </Card>
  );
};

export default RuleExtractor;