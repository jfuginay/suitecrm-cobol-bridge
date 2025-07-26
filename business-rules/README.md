# COBOL Business Rules Editor

A visual business rule editor for the SuiteCRM-COBOL Bridge project that makes complex COBOL business logic accessible to business analysts through intuitive visual decision trees and drag-and-drop editing.

## Features

### 1. Rule Extraction
- **COBOL Parser**: Automatically extracts business rules from COBOL PROCEDURE DIVISION code
- **Multiple Input Methods**: 
  - Manual code entry with syntax highlighting
  - File upload (.cob, .cbl, .cobol, .txt)
  - Pre-loaded sample rules for testing
- **Smart Detection**: Identifies IF/THEN/ELSE conditions, EVALUATE statements, and associated actions

### 2. Visual Decision Trees
- **Interactive Flowcharts**: Business rules displayed as visual decision trees
- **Automatic Layout**: Uses dagre algorithm for optimal node positioning
- **Node Types**:
  - Decision nodes (IF conditions) - Blue
  - Action nodes (MOVE, COMPUTE, PERFORM) - Green
  - Condition nodes (WHEN clauses) - Orange
  - Evaluate nodes (EVALUATE statements) - Purple
- **Pan & Zoom**: Full navigation controls with minimap
- **Real-time Updates**: Changes reflected immediately in the visual representation

### 3. Drag-and-Drop Rule Editor
- **Visual Editing**: Modify rules by interacting with the decision tree
- **Condition Builder**: Build complex conditions with AND/OR operators
- **Action Types**:
  - Assignment (MOVE statements)
  - Computation (COMPUTE statements)
  - Procedure calls (PERFORM)
  - Display messages
- **Node Operations**: Add, edit, delete nodes and connections
- **Validation**: Real-time validation of rule syntax

### 4. Test Case Generation
- **Automatic Test Data**: Generates test cases based on rule conditions
- **Test Execution**: Run tests against the rule engine
- **Coverage Analysis**: Ensures all branches are tested
- **Test Results**: Visual pass/fail indicators with detailed logs
- **Export Test Code**: Generate JavaScript/COBOL test scripts
- **Custom Test Cases**: Add manual test cases with expected outcomes

### 5. COBOL Code Export
- **Multiple Formats**:
  - COBOL code snippets
  - Complete PROCEDURE DIVISION
  - Human-readable documentation (Markdown)
  - JSON interchange format
- **Formatting Options**: Configurable indentation (2-8 spaces)
- **Version Control**: Track rule changes and history
- **Integration Ready**: Generated code can be directly integrated into COBOL programs

### 6. Rule Comparison & Versioning
- **Visual Diff**: Compare different versions of rules
- **Change Tracking**: See what changed between versions
- **Rollback**: Restore previous rule versions
- **Comments**: Add notes and documentation to rules

## Installation

```bash
# Clone the repository
git clone [repository-url]

# Navigate to the business rules editor
cd suitecrm-cobol-bridge/business-rules

# Install dependencies
npm install

# Start the development server
npm start
```

## Usage

### 1. Extract Rules from COBOL

```cobol
IF CUSTOMER-TYPE = "PREMIUM"
   IF ORDER-AMOUNT > 1000
      COMPUTE DISCOUNT-PERCENT = 15
   ELSE
      COMPUTE DISCOUNT-PERCENT = 10
   END-IF
ELSE
   IF CUSTOMER-TYPE = "REGULAR"
      IF ORDER-AMOUNT > 500
         COMPUTE DISCOUNT-PERCENT = 5
      ELSE
         COMPUTE DISCOUNT-PERCENT = 0
      END-IF
   END-IF
END-IF
```

Click "Extract Rules" to parse and visualize the business logic.

### 2. Edit Rules Visually

1. Click on any node in the decision tree
2. Use the rule editor panel to modify conditions or actions
3. Drag nodes to reorganize the flow
4. Add new branches or conditions

### 3. Generate Test Cases

1. Navigate to the "Test Cases" tab
2. Review automatically generated test cases
3. Add custom test scenarios
4. Run tests and verify results

### 4. Export Updated Rules

1. Go to the "Export" tab
2. Choose export format (COBOL, Documentation, JSON)
3. Configure formatting options
4. Download or copy the generated code

## Sample Business Rules

The editor includes several pre-loaded business rules:

1. **Customer Discount Calculation**: Determines discount based on customer type and order amount
2. **Credit Limit Validation**: Sets credit limits based on credit scores
3. **Order Processing Validation**: Validates orders before processing
4. **Shipping Cost Calculation**: Calculates shipping based on zone and weight

## Architecture

```
src/
├── components/
│   ├── RuleExtractor.js    # COBOL code input and parsing
│   ├── DecisionTree.js     # Visual flowchart rendering
│   ├── RuleEditor.js       # Drag-and-drop rule editing
│   ├── TestGenerator.js    # Test case generation and execution
│   ├── CobolExporter.js    # Code generation and export
│   └── CustomNode.js       # Custom React Flow node component
├── utils/
│   ├── cobolParser.js      # COBOL parsing logic
│   └── ruleEngine.js       # Rule execution engine
├── data/
│   └── sampleRules.json    # Pre-loaded example rules
└── App.js                  # Main application component
```

## Technologies Used

- **React**: UI framework
- **React Flow**: Interactive flowchart library
- **Ant Design**: UI component library
- **CodeMirror**: Code editor with syntax highlighting
- **Dagre**: Graph layout algorithm
- **File Saver**: Client-side file generation

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

This project is part of the SuiteCRM-COBOL Bridge and follows the same licensing terms.

## Support

For questions or issues:
- Check the documentation
- Review sample rules
- Submit an issue on GitHub