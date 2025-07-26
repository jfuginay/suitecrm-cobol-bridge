// COBOL Business Rule Parser Utilities

/**
 * Extracts business rules from COBOL code
 */
export const extractBusinessRules = (cobolCode) => {
  const rules = [];
  const lines = cobolCode.split('\n');
  
  let currentRule = null;
  let inProcedureDivision = false;
  let indentLevel = 0;
  
  lines.forEach((line, index) => {
    const trimmedLine = line.trim();
    
    // Track procedure division
    if (trimmedLine.includes('PROCEDURE DIVISION')) {
      inProcedureDivision = true;
      return;
    }
    
    if (!inProcedureDivision) return;
    
    // Extract IF conditions
    if (trimmedLine.startsWith('IF ')) {
      const condition = extractCondition(trimmedLine);
      currentRule = {
        id: `rule-${rules.length + 1}`,
        type: 'decision',
        condition: condition,
        trueBranch: [],
        falseBranch: [],
        lineNumber: index + 1,
        indentLevel: getIndentLevel(line)
      };
      rules.push(currentRule);
    }
    
    // Extract EVALUATE statements
    else if (trimmedLine.startsWith('EVALUATE ')) {
      const evaluateRule = extractEvaluateRule(lines, index);
      rules.push(evaluateRule);
    }
    
    // Extract PERFORM statements
    else if (trimmedLine.startsWith('PERFORM ')) {
      const action = extractAction(trimmedLine);
      if (currentRule) {
        currentRule.trueBranch.push(action);
      }
    }
    
    // Extract MOVE statements
    else if (trimmedLine.startsWith('MOVE ')) {
      const assignment = extractAssignment(trimmedLine);
      if (currentRule) {
        currentRule.trueBranch.push(assignment);
      }
    }
    
    // Extract COMPUTE statements
    else if (trimmedLine.startsWith('COMPUTE ')) {
      const computation = extractComputation(trimmedLine);
      if (currentRule) {
        currentRule.trueBranch.push(computation);
      }
    }
  });
  
  return rules;
};

/**
 * Extracts condition from IF statement
 */
const extractCondition = (line) => {
  const conditionMatch = line.match(/IF\s+(.+?)(?:\s+THEN)?$/i);
  if (conditionMatch) {
    return parseCondition(conditionMatch[1]);
  }
  return null;
};

/**
 * Parses COBOL condition into structured format
 */
const parseCondition = (conditionStr) => {
  // Handle compound conditions
  if (conditionStr.includes(' AND ')) {
    const parts = conditionStr.split(' AND ');
    return {
      type: 'compound',
      operator: 'AND',
      conditions: parts.map(part => parseSimpleCondition(part.trim()))
    };
  }
  
  if (conditionStr.includes(' OR ')) {
    const parts = conditionStr.split(' OR ');
    return {
      type: 'compound',
      operator: 'OR',
      conditions: parts.map(part => parseSimpleCondition(part.trim()))
    };
  }
  
  return parseSimpleCondition(conditionStr);
};

/**
 * Parses simple COBOL condition
 */
const parseSimpleCondition = (conditionStr) => {
  // Match patterns like: CUSTOMER-TYPE = "PREMIUM"
  const comparisonMatch = conditionStr.match(/(.+?)\s*(=|>|<|>=|<=|NOT=)\s*(.+)/);
  if (comparisonMatch) {
    return {
      type: 'comparison',
      field: comparisonMatch[1].trim(),
      operator: comparisonMatch[2].trim(),
      value: comparisonMatch[3].trim().replace(/['"]/g, '')
    };
  }
  
  // Match patterns like: NUMERIC(AMOUNT)
  const functionMatch = conditionStr.match(/(\w+)\((.+)\)/);
  if (functionMatch) {
    return {
      type: 'function',
      function: functionMatch[1],
      argument: functionMatch[2]
    };
  }
  
  // Match boolean flags
  return {
    type: 'boolean',
    field: conditionStr
  };
};

/**
 * Extracts EVALUATE rule structure
 */
const extractEvaluateRule = (lines, startIndex) => {
  const rule = {
    id: `rule-evaluate-${startIndex}`,
    type: 'evaluate',
    subject: '',
    cases: [],
    lineNumber: startIndex + 1
  };
  
  const evaluateLine = lines[startIndex].trim();
  const subjectMatch = evaluateLine.match(/EVALUATE\s+(.+)/i);
  if (subjectMatch) {
    rule.subject = subjectMatch[1].trim();
  }
  
  // Parse WHEN clauses
  for (let i = startIndex + 1; i < lines.length; i++) {
    const line = lines[i].trim();
    
    if (line.startsWith('END-EVALUATE')) break;
    
    if (line.startsWith('WHEN ')) {
      const whenMatch = line.match(/WHEN\s+(.+)/i);
      if (whenMatch) {
        const caseItem = {
          condition: whenMatch[1].trim(),
          actions: []
        };
        
        // Collect actions for this WHEN
        for (let j = i + 1; j < lines.length; j++) {
          const actionLine = lines[j].trim();
          if (actionLine.startsWith('WHEN ') || actionLine.startsWith('END-EVALUATE')) {
            break;
          }
          if (actionLine) {
            caseItem.actions.push(extractActionFromLine(actionLine));
          }
        }
        
        rule.cases.push(caseItem);
      }
    }
  }
  
  return rule;
};

/**
 * Extracts action from PERFORM statement
 */
const extractAction = (line) => {
  const performMatch = line.match(/PERFORM\s+(.+)/i);
  return {
    type: 'perform',
    procedure: performMatch ? performMatch[1].trim() : ''
  };
};

/**
 * Extracts assignment from MOVE statement
 */
const extractAssignment = (line) => {
  const moveMatch = line.match(/MOVE\s+(.+?)\s+TO\s+(.+)/i);
  if (moveMatch) {
    return {
      type: 'assignment',
      source: moveMatch[1].trim(),
      target: moveMatch[2].trim()
    };
  }
  return null;
};

/**
 * Extracts computation from COMPUTE statement
 */
const extractComputation = (line) => {
  const computeMatch = line.match(/COMPUTE\s+(.+?)\s*=\s*(.+)/i);
  if (computeMatch) {
    return {
      type: 'computation',
      target: computeMatch[1].trim(),
      expression: computeMatch[2].trim()
    };
  }
  return null;
};

/**
 * Extracts action from any line
 */
const extractActionFromLine = (line) => {
  if (line.startsWith('PERFORM ')) return extractAction(line);
  if (line.startsWith('MOVE ')) return extractAssignment(line);
  if (line.startsWith('COMPUTE ')) return extractComputation(line);
  if (line.startsWith('DISPLAY ')) {
    return {
      type: 'display',
      message: line.replace(/DISPLAY\s+/i, '').trim()
    };
  }
  return {
    type: 'unknown',
    statement: line
  };
};

/**
 * Gets indentation level of a line
 */
const getIndentLevel = (line) => {
  const match = line.match(/^(\s*)/);
  return match ? Math.floor(match[1].length / 4) : 0;
};

/**
 * Converts parsed rules to React Flow nodes and edges
 */
export const rulesToFlowElements = (rules) => {
  const nodes = [];
  const edges = [];
  let yPosition = 0;
  
  rules.forEach((rule, index) => {
    if (rule.type === 'decision') {
      // Create decision node
      const decisionNode = {
        id: rule.id,
        type: 'custom',
        position: { x: 250, y: yPosition },
        data: {
          label: formatCondition(rule.condition),
          type: 'decision',
          rule: rule
        }
      };
      nodes.push(decisionNode);
      
      // Create true branch nodes
      if (rule.trueBranch.length > 0) {
        rule.trueBranch.forEach((action, actionIndex) => {
          const actionNode = {
            id: `${rule.id}-true-${actionIndex}`,
            type: 'custom',
            position: { x: 100, y: yPosition + 100 + (actionIndex * 80) },
            data: {
              label: formatAction(action),
              type: 'action',
              action: action
            }
          };
          nodes.push(actionNode);
          
          edges.push({
            id: `edge-${rule.id}-true-${actionIndex}`,
            source: rule.id,
            target: actionNode.id,
            label: actionIndex === 0 ? 'True' : '',
            type: 'smoothstep',
            animated: true,
            style: { stroke: '#52c41a' }
          });
        });
      }
      
      // Create false branch nodes
      if (rule.falseBranch.length > 0) {
        rule.falseBranch.forEach((action, actionIndex) => {
          const actionNode = {
            id: `${rule.id}-false-${actionIndex}`,
            type: 'custom',
            position: { x: 400, y: yPosition + 100 + (actionIndex * 80) },
            data: {
              label: formatAction(action),
              type: 'action',
              action: action
            }
          };
          nodes.push(actionNode);
          
          edges.push({
            id: `edge-${rule.id}-false-${actionIndex}`,
            source: rule.id,
            target: actionNode.id,
            label: actionIndex === 0 ? 'False' : '',
            type: 'smoothstep',
            animated: true,
            style: { stroke: '#ff4d4f' }
          });
        });
      }
      
      yPosition += 250;
    }
    
    else if (rule.type === 'evaluate') {
      // Create evaluate node
      const evaluateNode = {
        id: rule.id,
        type: 'custom',
        position: { x: 250, y: yPosition },
        data: {
          label: `EVALUATE ${rule.subject}`,
          type: 'evaluate',
          rule: rule
        }
      };
      nodes.push(evaluateNode);
      
      // Create case nodes
      rule.cases.forEach((caseItem, caseIndex) => {
        const caseNode = {
          id: `${rule.id}-case-${caseIndex}`,
          type: 'custom',
          position: { x: 100 + (caseIndex * 200), y: yPosition + 100 },
          data: {
            label: `WHEN ${caseItem.condition}`,
            type: 'condition',
            case: caseItem
          }
        };
        nodes.push(caseNode);
        
        edges.push({
          id: `edge-${rule.id}-case-${caseIndex}`,
          source: rule.id,
          target: caseNode.id,
          type: 'smoothstep'
        });
        
        // Add action nodes for each case
        caseItem.actions.forEach((action, actionIndex) => {
          const actionNode = {
            id: `${rule.id}-case-${caseIndex}-action-${actionIndex}`,
            type: 'custom',
            position: { x: 100 + (caseIndex * 200), y: yPosition + 200 + (actionIndex * 80) },
            data: {
              label: formatAction(action),
              type: 'action',
              action: action
            }
          };
          nodes.push(actionNode);
          
          edges.push({
            id: `edge-${rule.id}-case-${caseIndex}-action-${actionIndex}`,
            source: caseNode.id,
            target: actionNode.id,
            type: 'smoothstep'
          });
        });
      });
      
      yPosition += 400;
    }
  });
  
  return { nodes, edges };
};

/**
 * Formats condition for display
 */
const formatCondition = (condition) => {
  if (!condition) return 'Unknown condition';
  
  if (condition.type === 'comparison') {
    return `${condition.field} ${condition.operator} ${condition.value}`;
  }
  
  if (condition.type === 'compound') {
    return condition.conditions
      .map(c => formatCondition(c))
      .join(` ${condition.operator} `);
  }
  
  if (condition.type === 'function') {
    return `${condition.function}(${condition.argument})`;
  }
  
  if (condition.type === 'boolean') {
    return condition.field;
  }
  
  return JSON.stringify(condition);
};

/**
 * Formats action for display
 */
const formatAction = (action) => {
  if (!action) return 'Unknown action';
  
  switch (action.type) {
    case 'perform':
      return `PERFORM ${action.procedure}`;
    case 'assignment':
      return `${action.target} = ${action.source}`;
    case 'computation':
      return `${action.target} = ${action.expression}`;
    case 'display':
      return `DISPLAY ${action.message}`;
    default:
      return action.statement || 'Unknown action';
  }
};