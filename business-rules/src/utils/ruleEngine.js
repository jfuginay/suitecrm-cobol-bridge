// Business Rule Execution Engine

/**
 * Executes a business rule against provided data
 */
export const executeRule = (rule, data) => {
  switch (rule.type) {
    case 'decision':
      return executeDecisionRule(rule, data);
    case 'evaluate':
      return executeEvaluateRule(rule, data);
    default:
      return { result: false, message: 'Unknown rule type' };
  }
};

/**
 * Executes a decision rule (IF statement)
 */
const executeDecisionRule = (rule, data) => {
  const conditionResult = evaluateCondition(rule.condition, data);
  const actions = conditionResult ? rule.trueBranch : rule.falseBranch;
  
  const results = [];
  const updates = {};
  
  actions.forEach(action => {
    const actionResult = executeAction(action, data, updates);
    results.push(actionResult);
  });
  
  return {
    result: conditionResult,
    branch: conditionResult ? 'true' : 'false',
    actions: results,
    updates: updates
  };
};

/**
 * Executes an evaluate rule (EVALUATE statement)
 */
const executeEvaluateRule = (rule, data) => {
  const subjectValue = getFieldValue(rule.subject, data);
  
  for (const caseItem of rule.cases) {
    if (matchesCase(caseItem.condition, subjectValue, data)) {
      const results = [];
      const updates = {};
      
      caseItem.actions.forEach(action => {
        const actionResult = executeAction(action, data, updates);
        results.push(actionResult);
      });
      
      return {
        result: true,
        matchedCase: caseItem.condition,
        actions: results,
        updates: updates
      };
    }
  }
  
  return {
    result: false,
    message: 'No matching case found'
  };
};

/**
 * Evaluates a condition against data
 */
const evaluateCondition = (condition, data) => {
  if (!condition) return false;
  
  switch (condition.type) {
    case 'comparison':
      return evaluateComparison(condition, data);
    case 'compound':
      return evaluateCompoundCondition(condition, data);
    case 'function':
      return evaluateFunctionCondition(condition, data);
    case 'boolean':
      return !!getFieldValue(condition.field, data);
    default:
      return false;
  }
};

/**
 * Evaluates a comparison condition
 */
const evaluateComparison = (condition, data) => {
  const fieldValue = getFieldValue(condition.field, data);
  const compareValue = condition.value;
  
  switch (condition.operator) {
    case '=':
    case '==':
      return fieldValue == compareValue;
    case 'NOT=':
    case '!=':
      return fieldValue != compareValue;
    case '>':
      return Number(fieldValue) > Number(compareValue);
    case '<':
      return Number(fieldValue) < Number(compareValue);
    case '>=':
      return Number(fieldValue) >= Number(compareValue);
    case '<=':
      return Number(fieldValue) <= Number(compareValue);
    default:
      return false;
  }
};

/**
 * Evaluates a compound condition (AND/OR)
 */
const evaluateCompoundCondition = (condition, data) => {
  const results = condition.conditions.map(c => evaluateCondition(c, data));
  
  if (condition.operator === 'AND') {
    return results.every(r => r === true);
  } else if (condition.operator === 'OR') {
    return results.some(r => r === true);
  }
  
  return false;
};

/**
 * Evaluates a function condition
 */
const evaluateFunctionCondition = (condition, data) => {
  const argument = getFieldValue(condition.argument, data);
  
  switch (condition.function.toUpperCase()) {
    case 'NUMERIC':
      return !isNaN(argument) && !isNaN(parseFloat(argument));
    case 'ALPHABETIC':
      return /^[a-zA-Z]+$/.test(argument);
    case 'ALPHANUMERIC':
      return /^[a-zA-Z0-9]+$/.test(argument);
    case 'POSITIVE':
      return Number(argument) > 0;
    case 'NEGATIVE':
      return Number(argument) < 0;
    case 'ZERO':
      return Number(argument) === 0;
    default:
      return false;
  }
};

/**
 * Executes an action
 */
const executeAction = (action, data, updates) => {
  switch (action.type) {
    case 'assignment':
      const value = getFieldValue(action.source, data);
      updates[action.target] = value;
      return { type: 'assignment', target: action.target, value: value };
      
    case 'computation':
      const result = evaluateExpression(action.expression, data);
      updates[action.target] = result;
      return { type: 'computation', target: action.target, result: result };
      
    case 'perform':
      return { type: 'perform', procedure: action.procedure };
      
    case 'display':
      return { type: 'display', message: action.message };
      
    default:
      return { type: 'unknown', action: action };
  }
};

/**
 * Gets field value from data
 */
const getFieldValue = (field, data) => {
  // Remove quotes if present
  if ((field.startsWith('"') && field.endsWith('"')) ||
      (field.startsWith("'") && field.endsWith("'"))) {
    return field.slice(1, -1);
  }
  
  // Handle nested fields (e.g., CUSTOMER.TYPE)
  const parts = field.split('.');
  let value = data;
  
  for (const part of parts) {
    if (value && typeof value === 'object') {
      value = value[part] || value[part.toLowerCase()] || value[part.toUpperCase()];
    } else {
      return field; // Return as literal if not found
    }
  }
  
  return value !== undefined ? value : field;
};

/**
 * Evaluates an expression
 */
const evaluateExpression = (expression, data) => {
  // Simple expression parser - handles basic arithmetic
  let expr = expression;
  
  // Replace field references with values
  const fieldPattern = /\b[A-Z][\w-]+\b/g;
  expr = expr.replace(fieldPattern, (match) => {
    const value = getFieldValue(match, data);
    return isNaN(value) ? `"${value}"` : value;
  });
  
  try {
    // Use Function constructor for safe evaluation
    const func = new Function('return ' + expr);
    return func();
  } catch (e) {
    console.error('Expression evaluation error:', e);
    return null;
  }
};

/**
 * Matches a case condition
 */
const matchesCase = (caseCondition, subjectValue, data) => {
  if (caseCondition === 'OTHER') return true;
  
  // Handle range conditions (e.g., "1 THRU 5")
  if (caseCondition.includes(' THRU ')) {
    const [start, end] = caseCondition.split(' THRU ').map(v => Number(v.trim()));
    const numValue = Number(subjectValue);
    return numValue >= start && numValue <= end;
  }
  
  // Handle multiple values (e.g., "1, 2, 3")
  if (caseCondition.includes(',')) {
    const values = caseCondition.split(',').map(v => v.trim());
    return values.includes(String(subjectValue));
  }
  
  // Direct comparison
  return String(subjectValue) === caseCondition;
};

/**
 * Generates test data based on rule conditions
 */
export const generateTestData = (rule) => {
  const testCases = [];
  
  if (rule.type === 'decision') {
    // Generate test case for true branch
    const trueCase = generateTestCaseForCondition(rule.condition, true);
    testCases.push({
      name: 'True branch test',
      data: trueCase,
      expectedBranch: 'true'
    });
    
    // Generate test case for false branch
    const falseCase = generateTestCaseForCondition(rule.condition, false);
    testCases.push({
      name: 'False branch test',
      data: falseCase,
      expectedBranch: 'false'
    });
  } else if (rule.type === 'evaluate') {
    // Generate test case for each WHEN clause
    rule.cases.forEach((caseItem, index) => {
      const testData = generateTestCaseForEvaluate(rule.subject, caseItem.condition);
      testCases.push({
        name: `Case ${index + 1}: WHEN ${caseItem.condition}`,
        data: testData,
        expectedCase: caseItem.condition
      });
    });
  }
  
  return testCases;
};

/**
 * Generates test data for a condition
 */
const generateTestCaseForCondition = (condition, shouldMatch) => {
  const testData = {};
  
  if (condition.type === 'comparison') {
    const field = condition.field;
    const value = condition.value;
    
    if (shouldMatch) {
      testData[field] = value;
    } else {
      // Generate a different value
      if (!isNaN(value)) {
        testData[field] = Number(value) + 1;
      } else {
        testData[field] = value + '_different';
      }
    }
  } else if (condition.type === 'compound') {
    // For compound conditions, recursively generate data
    condition.conditions.forEach(subCondition => {
      const subData = generateTestCaseForCondition(subCondition, shouldMatch);
      Object.assign(testData, subData);
    });
  }
  
  return testData;
};

/**
 * Generates test data for EVALUATE cases
 */
const generateTestCaseForEvaluate = (subject, caseCondition) => {
  const testData = {};
  
  if (caseCondition === 'OTHER') {
    testData[subject] = 'other_value';
  } else if (caseCondition.includes(' THRU ')) {
    const [start, end] = caseCondition.split(' THRU ').map(v => Number(v.trim()));
    testData[subject] = Math.floor((start + end) / 2);
  } else if (caseCondition.includes(',')) {
    const values = caseCondition.split(',').map(v => v.trim());
    testData[subject] = values[0];
  } else {
    testData[subject] = caseCondition;
  }
  
  return testData;
};