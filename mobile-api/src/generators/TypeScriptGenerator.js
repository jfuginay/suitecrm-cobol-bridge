const winston = require('winston');

class TypeScriptGenerator {
  constructor() {
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });

    // TypeScript type mappings for COBOL data types
    this.typeMapping = {
      'X': 'string',
      'A': 'string',
      '9': 'number',
      'S9': 'number',
      'V9': 'number',
      'COMP': 'number',
      'COMP-3': 'number',
      'BINARY': 'number',
      'POINTER': 'string',
    };
  }

  /**
   * Generate TypeScript interfaces from COPYBOOK
   * @param {string} copybook - COBOL COPYBOOK definition
   * @param {string} namespace - TypeScript namespace
   * @returns {string} TypeScript definitions
   */
  async generateFromCopybook(copybook, namespace = 'CobolTypes') {
    try {
      const structures = this.parseCopybook(copybook);
      const interfaces = this.generateInterfaces(structures);
      const validators = this.generateValidators(structures);
      const converters = this.generateConverters(structures);
      
      return this.wrapInNamespace(namespace, interfaces, validators, converters);
    } catch (error) {
      this.logger.error('TypeScript generation error:', error);
      throw error;
    }
  }

  /**
   * Parse COPYBOOK into structure definitions
   */
  parseCopybook(copybook) {
    const structures = [];
    const lines = copybook.split('\n');
    let currentStructure = null;
    let currentGroup = null;
    const levelStack = [];
    
    lines.forEach((line, index) => {
      // Skip comments and empty lines
      if (line.trim().startsWith('*') || !line.trim()) {
        return;
      }

      // Parse field definition
      const fieldMatch = line.match(/^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+(.+))?\.?\s*$/);
      
      if (fieldMatch) {
        const [, level, name, definition] = fieldMatch;
        const levelNum = parseInt(level);
        
        // Handle record level (01)
        if (levelNum === 1) {
          if (currentStructure) {
            structures.push(currentStructure);
          }
          currentStructure = {
            name: this.pascalCase(name),
            originalName: name,
            fields: [],
            level: levelNum
          };
          levelStack.length = 0;
          levelStack.push(currentStructure);
        }
        // Handle field levels
        else if (currentStructure) {
          const field = this.parseField(levelNum, name, definition);
          
          // Find parent based on level
          while (levelStack.length > 0 && levelStack[levelStack.length - 1].level >= levelNum) {
            levelStack.pop();
          }
          
          const parent = levelStack[levelStack.length - 1];
          
          if (field.isGroup) {
            field.fields = [];
            levelStack.push(field);
          }
          
          if (parent) {
            if (parent.fields) {
              parent.fields.push(field);
            } else {
              parent.fields = [field];
            }
          }
        }
      }
    });
    
    // Add last structure
    if (currentStructure) {
      structures.push(currentStructure);
    }
    
    return structures;
  }

  /**
   * Parse individual field
   */
  parseField(level, name, definition) {
    const field = {
      level,
      name: this.camelCase(name),
      originalName: name,
      isGroup: false,
      occurs: null,
      redefines: null,
      picClause: null,
      type: 'any',
      isOptional: false,
      defaultValue: null,
      validations: {}
    };

    if (!definition || !definition.trim()) {
      field.isGroup = true;
      return field;
    }

    const defParts = definition.toUpperCase();

    // Check for OCCURS
    const occursMatch = defParts.match(/OCCURS\s+(\d+)(?:\s+TIMES)?/);
    if (occursMatch) {
      field.occurs = parseInt(occursMatch[1]);
    }

    // Check for REDEFINES
    const redefinesMatch = defParts.match(/REDEFINES\s+([A-Z0-9-]+)/);
    if (redefinesMatch) {
      field.redefines = this.camelCase(redefinesMatch[1]);
    }

    // Check for PIC clause
    const picMatch = defParts.match(/PIC(?:TURE)?\s+([^\s]+)/);
    if (picMatch) {
      field.picClause = picMatch[1];
      field.type = this.getTypeFromPic(picMatch[1]);
      field.validations = this.getValidationsFromPic(picMatch[1]);
    } else {
      field.isGroup = true;
    }

    // Check for VALUE clause
    const valueMatch = defParts.match(/VALUE\s+([^\s]+)/);
    if (valueMatch) {
      field.defaultValue = this.parseValue(valueMatch[1], field.type);
    }

    // Check for optional indicators
    if (defParts.includes('DEPENDING ON')) {
      field.isOptional = true;
    }

    return field;
  }

  /**
   * Generate TypeScript interfaces
   */
  generateInterfaces(structures) {
    let code = '';
    
    structures.forEach(structure => {
      code += this.generateInterface(structure) + '\n\n';
    });
    
    return code;
  }

  /**
   * Generate single interface
   */
  generateInterface(structure) {
    let code = `/**\n * Generated from COBOL structure: ${structure.originalName}\n */\n`;
    code += `export interface ${structure.name} {\n`;
    
    structure.fields.forEach(field => {
      code += this.generateFieldDefinition(field, 1);
    });
    
    code += '}';
    
    return code;
  }

  /**
   * Generate field definition
   */
  generateFieldDefinition(field, indent = 1) {
    const indentStr = '  '.repeat(indent);
    let code = '';
    
    // Add JSDoc comment
    code += `${indentStr}/**\n`;
    code += `${indentStr} * ${field.originalName}`;
    if (field.picClause) {
      code += ` - PIC ${field.picClause}`;
    }
    if (field.occurs) {
      code += ` - OCCURS ${field.occurs}`;
    }
    code += `\n${indentStr} */\n`;
    
    // Generate field
    const optional = field.isOptional ? '?' : '';
    
    if (field.isGroup && field.fields) {
      // Group field - generate nested interface
      code += `${indentStr}${field.name}${optional}: {\n`;
      field.fields.forEach(subField => {
        code += this.generateFieldDefinition(subField, indent + 1);
      });
      code += `${indentStr}}`;
    } else if (field.occurs) {
      // Array field
      code += `${indentStr}${field.name}${optional}: ${field.type}[]`;
    } else {
      // Simple field
      code += `${indentStr}${field.name}${optional}: ${field.type}`;
    }
    
    code += ';\n';
    
    return code;
  }

  /**
   * Generate validators
   */
  generateValidators(structures) {
    let code = '/**\n * Field validators\n */\n';
    code += 'export const validators = {\n';
    
    structures.forEach(structure => {
      code += `  ${structure.name}: {\n`;
      this.generateFieldValidators(structure.fields, code, 2);
      code += '  },\n';
    });
    
    code += '};\n\n';
    
    // Generate validation function
    code += `/**\n * Validate a record against its COBOL definition\n */\n`;
    code += `export function validate<T extends keyof typeof validators>(\n`;
    code += `  recordType: T,\n`;
    code += `  data: any\n`;
    code += `): { isValid: boolean; errors: Record<string, string> } {\n`;
    code += `  const errors: Record<string, string> = {};\n`;
    code += `  const validator = validators[recordType];\n\n`;
    code += `  if (!validator) {\n`;
    code += `    return { isValid: false, errors: { _: 'Unknown record type' } };\n`;
    code += `  }\n\n`;
    code += `  // Validate each field\n`;
    code += `  for (const [fieldName, rules] of Object.entries(validator)) {\n`;
    code += `    const value = data[fieldName];\n`;
    code += `    const fieldErrors = validateField(value, rules as any);\n`;
    code += `    if (fieldErrors.length > 0) {\n`;
    code += `      errors[fieldName] = fieldErrors.join(', ');\n`;
    code += `    }\n`;
    code += `  }\n\n`;
    code += `  return {\n`;
    code += `    isValid: Object.keys(errors).length === 0,\n`;
    code += `    errors\n`;
    code += `  };\n`;
    code += `}\n\n`;
    
    // Generate field validation function
    code += this.generateFieldValidationFunction();
    
    return code;
  }

  /**
   * Generate field validators recursively
   */
  generateFieldValidators(fields, code, indent) {
    const indentStr = '  '.repeat(indent);
    
    fields.forEach(field => {
      if (field.isGroup && field.fields) {
        code += `${indentStr}${field.name}: {\n`;
        this.generateFieldValidators(field.fields, code, indent + 1);
        code += `${indentStr}},\n`;
      } else if (Object.keys(field.validations).length > 0) {
        code += `${indentStr}${field.name}: ${JSON.stringify(field.validations)},\n`;
      }
    });
  }

  /**
   * Generate field validation function
   */
  generateFieldValidationFunction() {
    return `function validateField(value: any, rules: any): string[] {
  const errors: string[] = [];
  
  if (rules.required && (value === null || value === undefined || value === '')) {
    errors.push('Field is required');
  }
  
  if (value !== null && value !== undefined) {
    if (rules.type === 'number' && typeof value !== 'number') {
      errors.push('Must be a number');
    }
    
    if (rules.type === 'string' && typeof value !== 'string') {
      errors.push('Must be a string');
    }
    
    if (rules.maxLength && value.toString().length > rules.maxLength) {
      errors.push(\`Maximum length is \${rules.maxLength}\`);
    }
    
    if (rules.minValue !== undefined && value < rules.minValue) {
      errors.push(\`Minimum value is \${rules.minValue}\`);
    }
    
    if (rules.maxValue !== undefined && value > rules.maxValue) {
      errors.push(\`Maximum value is \${rules.maxValue}\`);
    }
    
    if (rules.pattern && !new RegExp(rules.pattern).test(value.toString())) {
      errors.push('Invalid format');
    }
    
    if (rules.decimals !== undefined && typeof value === 'number') {
      const decimalPlaces = (value.toString().split('.')[1] || '').length;
      if (decimalPlaces > rules.decimals) {
        errors.push(\`Maximum \${rules.decimals} decimal places allowed\`);
      }
    }
  }
  
  return errors;
}\n\n`;
  }

  /**
   * Generate converters
   */
  generateConverters(structures) {
    let code = '/**\n * Data converters for COBOL integration\n */\n\n';
    
    // Generate to COBOL converter
    code += `/**\n * Convert TypeScript object to COBOL format\n */\n`;
    code += `export function toCobolFormat<T extends keyof typeof validators>(\n`;
    code += `  recordType: T,\n`;
    code += `  data: any\n`;
    code += `): Record<string, any> {\n`;
    code += `  const result: Record<string, any> = {};\n`;
    code += `  const structure = validators[recordType];\n\n`;
    code += `  for (const [field, rules] of Object.entries(structure)) {\n`;
    code += `    const value = data[field];\n`;
    code += `    if (value !== undefined) {\n`;
    code += `      result[field] = formatForCobol(value, rules as any);\n`;
    code += `    }\n`;
    code += `  }\n\n`;
    code += `  return result;\n`;
    code += `}\n\n`;
    
    // Generate from COBOL converter
    code += `/**\n * Convert COBOL data to TypeScript format\n */\n`;
    code += `export function fromCobolFormat<T extends keyof typeof validators>(\n`;
    code += `  recordType: T,\n`;
    code += `  data: any\n`;
    code += `): any {\n`;
    code += `  const result: any = {};\n`;
    code += `  const structure = validators[recordType];\n\n`;
    code += `  for (const [field, rules] of Object.entries(structure)) {\n`;
    code += `    const value = data[field];\n`;
    code += `    if (value !== undefined) {\n`;
    code += `      result[field] = parseFromCobol(value, rules as any);\n`;
    code += `    }\n`;
    code += `  }\n\n`;
    code += `  return result;\n`;
    code += `}\n\n`;
    
    // Helper functions
    code += this.generateConverterHelpers();
    
    return code;
  }

  /**
   * Generate converter helper functions
   */
  generateConverterHelpers() {
    return `function formatForCobol(value: any, rules: any): any {
  if (rules.type === 'number') {
    const num = Number(value) || 0;
    if (rules.decimals) {
      return num.toFixed(rules.decimals).replace('.', '');
    }
    return Math.round(num).toString().padStart(rules.maxLength || 1, '0');
  } else if (rules.type === 'string') {
    return String(value).padEnd(rules.maxLength || 1, ' ').substring(0, rules.maxLength);
  }
  return value;
}

function parseFromCobol(value: any, rules: any): any {
  if (rules.type === 'number') {
    const str = String(value).trim();
    if (rules.decimals) {
      const intPart = str.slice(0, -rules.decimals) || '0';
      const decPart = str.slice(-rules.decimals);
      return parseFloat(\`\${intPart}.\${decPart}\`);
    }
    return parseInt(str) || 0;
  } else if (rules.type === 'string') {
    return String(value).trimEnd();
  }
  return value;
}\n\n`;
  }

  /**
   * Wrap in namespace
   */
  wrapInNamespace(namespace, interfaces, validators, converters) {
    let code = `/**
 * Auto-generated TypeScript definitions from COBOL COPYBOOK
 * Generated on: ${new Date().toISOString()}
 */

export namespace ${namespace} {
${this.indentCode(interfaces, 1)}
${this.indentCode(validators, 1)}
${this.indentCode(converters, 1)}
  /**
   * Export all interfaces for external use
   */
  export type RecordTypes = keyof typeof validators;
}

// Re-export for convenience
export const ${namespace}Validators = ${namespace}.validators;
export const ${namespace}Validate = ${namespace}.validate;
export const ${namespace}ToCobol = ${namespace}.toCobolFormat;
export const ${namespace}FromCobol = ${namespace}.fromCobolFormat;
`;
    
    return code;
  }

  /**
   * Helper methods
   */
  getTypeFromPic(picClause) {
    if (picClause.includes('9') || picClause.includes('S') || picClause.includes('V')) {
      return 'number';
    }
    if (picClause.includes('X') || picClause.includes('A')) {
      return 'string';
    }
    return 'any';
  }

  getValidationsFromPic(picClause) {
    const validations = {};
    
    // Determine type
    if (picClause.includes('9')) {
      validations.type = 'number';
      
      // Extract length
      const lengthMatch = picClause.match(/9\((\d+)\)/);
      if (lengthMatch) {
        const digits = parseInt(lengthMatch[1]);
        validations.maxValue = Math.pow(10, digits) - 1;
        validations.minValue = picClause.includes('S') ? -validations.maxValue : 0;
      }
      
      // Extract decimals
      const decimalMatch = picClause.match(/V9\((\d+)\)/);
      if (decimalMatch) {
        validations.decimals = parseInt(decimalMatch[1]);
      }
    } else if (picClause.includes('X')) {
      validations.type = 'string';
      
      // Extract length
      const lengthMatch = picClause.match(/X\((\d+)\)/);
      if (lengthMatch) {
        validations.maxLength = parseInt(lengthMatch[1]);
      }
    } else if (picClause.includes('A')) {
      validations.type = 'string';
      validations.pattern = '^[A-Za-z\\s]*$';
      
      // Extract length
      const lengthMatch = picClause.match(/A\((\d+)\)/);
      if (lengthMatch) {
        validations.maxLength = parseInt(lengthMatch[1]);
      }
    }
    
    return validations;
  }

  parseValue(value, type) {
    if (value === 'SPACES') return '';
    if (value === 'ZEROS' || value === 'ZERO') return type === 'number' ? 0 : '0';
    if (value === 'LOW-VALUES') return type === 'number' ? 0 : '\x00';
    if (value === 'HIGH-VALUES') return type === 'number' ? 999999999 : '\xFF';
    
    // Remove quotes
    if (value.startsWith('"') && value.endsWith('"')) {
      return value.slice(1, -1);
    }
    if (value.startsWith("'") && value.endsWith("'")) {
      return value.slice(1, -1);
    }
    
    // Parse number
    if (type === 'number' && !isNaN(value)) {
      return parseFloat(value);
    }
    
    return value;
  }

  camelCase(str) {
    return str.toLowerCase().replace(/-([a-z])/g, (g) => g[1].toUpperCase());
  }

  pascalCase(str) {
    const camel = this.camelCase(str);
    return camel.charAt(0).toUpperCase() + camel.slice(1);
  }

  indentCode(code, level) {
    const indent = '  '.repeat(level);
    return code.split('\n').map(line => line ? indent + line : line).join('\n');
  }

  /**
   * Generate complete TypeScript module from multiple COPYBOOKS
   */
  async generateModule(copybooks, moduleName = 'CobolTypes') {
    const namespaces = [];
    
    for (const [name, copybook] of Object.entries(copybooks)) {
      const namespace = this.pascalCase(name);
      const code = await this.generateFromCopybook(copybook, namespace);
      namespaces.push(code);
    }
    
    const moduleCode = `/**
 * COBOL TypeScript Definitions Module
 * Auto-generated from COPYBOOK definitions
 * Generated on: ${new Date().toISOString()}
 */

${namespaces.join('\n\n')}

/**
 * Consolidated exports
 */
export const CobolTypes = {
${Object.keys(copybooks).map(name => {
  const ns = this.pascalCase(name);
  return `  ${ns}: {
    validators: ${ns}Validators,
    validate: ${ns}Validate,
    toCobol: ${ns}ToCobol,
    fromCobol: ${ns}FromCobol
  }`;
}).join(',\n')}
};

export default CobolTypes;
`;
    
    return moduleCode;
  }
}

module.exports = TypeScriptGenerator;