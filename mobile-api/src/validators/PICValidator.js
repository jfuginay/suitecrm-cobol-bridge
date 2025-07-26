const winston = require('winston');

class PICValidator {
  constructor() {
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });

    // PIC clause patterns
    this.patterns = {
      numeric: /^S?9+(?:\((\d+)\))?(?:V9+(?:\((\d+)\))?)?$/,
      alphanumeric: /^X+(?:\((\d+)\))?$/,
      alphabetic: /^A+(?:\((\d+)\))?$/,
      signed: /^S9/,
      decimal: /V9/,
      comp: /COMP(?:-3)?$/,
    };
  }

  /**
   * Validate value against PIC clause
   * @param {any} value - Value to validate
   * @param {string} picClause - COBOL PIC clause
   * @param {string} fieldName - Field name for error messages
   * @returns {object} Validation result
   */
  validate(value, picClause, fieldName = 'field') {
    try {
      const cleanPic = picClause.replace(/\s+/g, '').toUpperCase();
      
      // Extract format and constraints
      const format = this.parsePicClause(cleanPic);
      
      // Perform validation based on type
      switch (format.type) {
        case 'numeric':
          return this.validateNumeric(value, format, fieldName);
        case 'alphanumeric':
          return this.validateAlphanumeric(value, format, fieldName);
        case 'alphabetic':
          return this.validateAlphabetic(value, format, fieldName);
        default:
          return {
            isValid: false,
            error: `Unknown PIC clause format: ${picClause}`,
            fieldName
          };
      }
    } catch (error) {
      this.logger.error('Validation error:', error);
      return {
        isValid: false,
        error: error.message,
        fieldName
      };
    }
  }

  /**
   * Parse PIC clause into format object
   */
  parsePicClause(picClause) {
    const format = {
      type: null,
      length: 0,
      decimals: 0,
      signed: false,
      comp: false,
      comp3: false
    };

    // Check for COMP types
    if (picClause.includes('COMP-3')) {
      format.comp3 = true;
      picClause = picClause.replace('COMP-3', '').trim();
    } else if (picClause.includes('COMP')) {
      format.comp = true;
      picClause = picClause.replace('COMP', '').trim();
    }

    // Check for numeric
    if (this.patterns.numeric.test(picClause)) {
      format.type = 'numeric';
      format.signed = this.patterns.signed.test(picClause);
      
      // Extract integer length
      const intMatch = picClause.match(/S?9+(?:\((\d+)\))?/);
      if (intMatch) {
        if (intMatch[1]) {
          format.length = parseInt(intMatch[1]);
        } else {
          format.length = (intMatch[0].match(/9/g) || []).length;
        }
      }
      
      // Extract decimal length
      const decMatch = picClause.match(/V9+(?:\((\d+)\))?/);
      if (decMatch) {
        if (decMatch[1]) {
          format.decimals = parseInt(decMatch[1]);
        } else {
          format.decimals = (decMatch[0].match(/9/g) || []).length;
        }
      }
    }
    // Check for alphanumeric
    else if (this.patterns.alphanumeric.test(picClause)) {
      format.type = 'alphanumeric';
      const match = picClause.match(/X+(?:\((\d+)\))?/);
      if (match) {
        if (match[1]) {
          format.length = parseInt(match[1]);
        } else {
          format.length = (match[0].match(/X/g) || []).length;
        }
      }
    }
    // Check for alphabetic
    else if (this.patterns.alphabetic.test(picClause)) {
      format.type = 'alphabetic';
      const match = picClause.match(/A+(?:\((\d+)\))?/);
      if (match) {
        if (match[1]) {
          format.length = parseInt(match[1]);
        } else {
          format.length = (match[0].match(/A/g) || []).length;
        }
      }
    }

    return format;
  }

  /**
   * Validate numeric value
   */
  validateNumeric(value, format, fieldName) {
    const result = {
      isValid: true,
      error: null,
      fieldName,
      formattedValue: null
    };

    // Check if value is numeric
    const numValue = parseFloat(value);
    if (isNaN(numValue)) {
      result.isValid = false;
      result.error = `${fieldName} must be numeric`;
      return result;
    }

    // Check sign
    if (!format.signed && numValue < 0) {
      result.isValid = false;
      result.error = `${fieldName} cannot be negative`;
      return result;
    }

    // Check integer part length
    const intPart = Math.floor(Math.abs(numValue));
    const intLength = intPart.toString().length;
    if (intLength > format.length) {
      result.isValid = false;
      result.error = `${fieldName} integer part exceeds maximum length of ${format.length}`;
      return result;
    }

    // Check decimal places
    if (format.decimals > 0) {
      const decimalPart = (numValue % 1).toFixed(format.decimals).substring(2);
      if (decimalPart.length > format.decimals) {
        result.isValid = false;
        result.error = `${fieldName} decimal places exceed maximum of ${format.decimals}`;
        return result;
      }
      result.formattedValue = numValue.toFixed(format.decimals);
    } else {
      // No decimals allowed
      if (numValue % 1 !== 0) {
        result.isValid = false;
        result.error = `${fieldName} cannot have decimal places`;
        return result;
      }
      result.formattedValue = numValue.toString();
    }

    // Check total digits for COMP-3
    if (format.comp3) {
      const totalDigits = format.length + format.decimals;
      const valueDigits = result.formattedValue.replace(/[.-]/g, '').length;
      if (valueDigits > totalDigits) {
        result.isValid = false;
        result.error = `${fieldName} exceeds maximum of ${totalDigits} digits for COMP-3`;
        return result;
      }
    }

    return result;
  }

  /**
   * Validate alphanumeric value
   */
  validateAlphanumeric(value, format, fieldName) {
    const result = {
      isValid: true,
      error: null,
      fieldName,
      formattedValue: null
    };

    const strValue = String(value);

    // Check length
    if (strValue.length > format.length) {
      result.isValid = false;
      result.error = `${fieldName} exceeds maximum length of ${format.length}`;
      return result;
    }

    // Alphanumeric can contain any character
    result.formattedValue = strValue.padEnd(format.length, ' ');

    return result;
  }

  /**
   * Validate alphabetic value
   */
  validateAlphabetic(value, format, fieldName) {
    const result = {
      isValid: true,
      error: null,
      fieldName,
      formattedValue: null
    };

    const strValue = String(value);

    // Check length
    if (strValue.length > format.length) {
      result.isValid = false;
      result.error = `${fieldName} exceeds maximum length of ${format.length}`;
      return result;
    }

    // Check for non-alphabetic characters
    if (!/^[A-Za-z\s]*$/.test(strValue)) {
      result.isValid = false;
      result.error = `${fieldName} can only contain letters and spaces`;
      return result;
    }

    result.formattedValue = strValue.padEnd(format.length, ' ');

    return result;
  }

  /**
   * Validate an entire record against a COPYBOOK definition
   * @param {object} record - Data record
   * @param {Array} fields - Array of field definitions with PIC clauses
   * @returns {object} Validation results
   */
  validateRecord(record, fields) {
    const results = {
      isValid: true,
      errors: {},
      formattedRecord: {}
    };

    fields.forEach(field => {
      const value = record[field.name];
      
      // Skip optional fields if not provided
      if (value === undefined && !field.required) {
        return;
      }

      // Check required fields
      if (field.required && (value === undefined || value === null || value === '')) {
        results.isValid = false;
        results.errors[field.name] = `${field.name} is required`;
        return;
      }

      // Validate against PIC clause
      const validation = this.validate(value, field.picClause, field.name);
      
      if (!validation.isValid) {
        results.isValid = false;
        results.errors[field.name] = validation.error;
      } else {
        results.formattedRecord[field.name] = validation.formattedValue || value;
      }
    });

    return results;
  }

  /**
   * Get validation rules for a PIC clause (for client-side validation)
   * @param {string} picClause - COBOL PIC clause
   * @returns {object} Validation rules
   */
  getValidationRules(picClause) {
    const format = this.parsePicClause(picClause.toUpperCase());
    const rules = {};

    switch (format.type) {
      case 'numeric':
        rules.type = 'number';
        rules.maxLength = format.length + format.decimals + (format.decimals > 0 ? 1 : 0) + (format.signed ? 1 : 0);
        rules.pattern = format.signed ? 
          (format.decimals > 0 ? /^-?\d+(\.\d+)?$/ : /^-?\d+$/) :
          (format.decimals > 0 ? /^\d+(\.\d+)?$/ : /^\d+$/);
        rules.min = format.signed ? -Math.pow(10, format.length) + 1 : 0;
        rules.max = Math.pow(10, format.length) - 1;
        rules.decimals = format.decimals;
        rules.allowNegative = format.signed;
        break;

      case 'alphanumeric':
        rules.type = 'text';
        rules.maxLength = format.length;
        rules.pattern = /^[\x20-\x7E]*$/; // Printable ASCII
        break;

      case 'alphabetic':
        rules.type = 'text';
        rules.maxLength = format.length;
        rules.pattern = /^[A-Za-z\s]*$/;
        break;
    }

    rules.picClause = picClause;
    rules.format = format;

    return rules;
  }

  /**
   * Format value according to PIC clause for display
   * @param {any} value - Value to format
   * @param {string} picClause - COBOL PIC clause
   * @returns {string} Formatted value
   */
  formatForDisplay(value, picClause) {
    const format = this.parsePicClause(picClause.toUpperCase());

    if (format.type === 'numeric') {
      const numValue = parseFloat(value) || 0;
      
      if (format.decimals > 0) {
        return numValue.toFixed(format.decimals);
      } else {
        return Math.round(numValue).toString();
      }
    }

    return String(value);
  }

  /**
   * Generate input mask for PIC clause
   * @param {string} picClause - COBOL PIC clause
   * @returns {string} Input mask pattern
   */
  generateInputMask(picClause) {
    const format = this.parsePicClause(picClause.toUpperCase());

    switch (format.type) {
      case 'numeric':
        let mask = '9'.repeat(format.length);
        if (format.decimals > 0) {
          mask += '.' + '9'.repeat(format.decimals);
        }
        if (format.signed) {
          mask = '-' + mask;
        }
        return mask;

      case 'alphanumeric':
        return 'X'.repeat(format.length);

      case 'alphabetic':
        return 'A'.repeat(format.length);

      default:
        return '';
    }
  }
}

module.exports = PICValidator;