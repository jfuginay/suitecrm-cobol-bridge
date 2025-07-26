/**
 * Auto-generated TypeScript definitions from COBOL COPYBOOK
 * Generated on: 2024-01-15T10:00:00.000Z
 */

export namespace CobolTypes {
  /**
   * Generated from COBOL structure: CUSTOMER-RECORD
   */
  export interface CustomerRecord {
    /**
     * CUST-ID - PIC 9(10)
     */
    custId: number;
    /**
     * CUST-NAME - PIC X(30)
     */
    custName: string;
    /**
     * CUST-ADDRESS
     */
    custAddress: {
      /**
       * STREET - PIC X(30)
       */
      street: string;
      /**
       * CITY - PIC X(20)
       */
      city: string;
      /**
       * STATE - PIC X(2)
       */
      state: string;
      /**
       * ZIP - PIC 9(5)
       */
      zip: number;
    };
    /**
     * CUST-BALANCE - PIC S9(7)V99 - COMP-3
     */
    custBalance: number;
    /**
     * CUST-STATUS - PIC X(1)
     */
    custStatus?: string;
    /**
     * CUST-TRANSACTIONS - OCCURS 10
     */
    custTransactions?: Transaction[];
  }

  /**
   * Generated from COBOL structure: TRANSACTION
   */
  export interface Transaction {
    /**
     * TRANS-ID - PIC 9(10)
     */
    transId: number;
    /**
     * TRANS-DATE - PIC 9(8)
     */
    transDate: number;
    /**
     * TRANS-AMOUNT - PIC S9(7)V99
     */
    transAmount: number;
    /**
     * TRANS-TYPE - PIC X(2)
     */
    transType: string;
    /**
     * TRANS-DESC - PIC X(50)
     */
    transDesc: string;
  }

  /**
   * Generated from COBOL structure: ACCOUNT-RECORD
   */
  export interface AccountRecord {
    /**
     * ACCT-NUMBER - PIC 9(10)
     */
    acctNumber: number;
    /**
     * ACCT-TYPE - PIC X(2)
     */
    acctType: string;
    /**
     * ACCT-BALANCE - PIC S9(9)V99
     */
    acctBalance: number;
    /**
     * ACCT-OPENED-DATE - PIC 9(8)
     */
    acctOpenedDate: number;
    /**
     * ACCT-LAST-ACTIVITY - PIC 9(8)
     */
    acctLastActivity: number;
    /**
     * ACCT-STATUS - PIC X(1)
     */
    acctStatus: string;
  }

  /**
   * Field validators
   */
  export const validators = {
    CustomerRecord: {
      custId: { type: 'number', maxValue: 9999999999, minValue: 0 },
      custName: { type: 'string', maxLength: 30 },
      custAddress: {
        street: { type: 'string', maxLength: 30 },
        city: { type: 'string', maxLength: 20 },
        state: { type: 'string', maxLength: 2 },
        zip: { type: 'number', maxValue: 99999, minValue: 0 },
      },
      custBalance: { type: 'number', maxValue: 9999999.99, minValue: -9999999.99, decimals: 2 },
      custStatus: { type: 'string', maxLength: 1 },
    },
    Transaction: {
      transId: { type: 'number', maxValue: 9999999999, minValue: 0 },
      transDate: { type: 'number', maxValue: 99999999, minValue: 0 },
      transAmount: { type: 'number', maxValue: 9999999.99, minValue: -9999999.99, decimals: 2 },
      transType: { type: 'string', maxLength: 2 },
      transDesc: { type: 'string', maxLength: 50 },
    },
    AccountRecord: {
      acctNumber: { type: 'number', maxValue: 9999999999, minValue: 0 },
      acctType: { type: 'string', maxLength: 2 },
      acctBalance: { type: 'number', maxValue: 999999999.99, minValue: -999999999.99, decimals: 2 },
      acctOpenedDate: { type: 'number', maxValue: 99999999, minValue: 0 },
      acctLastActivity: { type: 'number', maxValue: 99999999, minValue: 0 },
      acctStatus: { type: 'string', maxLength: 1 },
    },
  };

  /**
   * Validate a record against its COBOL definition
   */
  export function validate<T extends keyof typeof validators>(
    recordType: T,
    data: any
  ): { isValid: boolean; errors: Record<string, string> } {
    const errors: Record<string, string> = {};
    const validator = validators[recordType];

    if (!validator) {
      return { isValid: false, errors: { _: 'Unknown record type' } };
    }

    // Validate each field
    for (const [fieldName, rules] of Object.entries(validator)) {
      const value = data[fieldName];
      const fieldErrors = validateField(value, rules as any);
      if (fieldErrors.length > 0) {
        errors[fieldName] = fieldErrors.join(', ');
      }
    }

    return {
      isValid: Object.keys(errors).length === 0,
      errors
    };
  }

  function validateField(value: any, rules: any): string[] {
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
        errors.push(`Maximum length is ${rules.maxLength}`);
      }
      
      if (rules.minValue !== undefined && value < rules.minValue) {
        errors.push(`Minimum value is ${rules.minValue}`);
      }
      
      if (rules.maxValue !== undefined && value > rules.maxValue) {
        errors.push(`Maximum value is ${rules.maxValue}`);
      }
      
      if (rules.pattern && !new RegExp(rules.pattern).test(value.toString())) {
        errors.push('Invalid format');
      }
      
      if (rules.decimals !== undefined && typeof value === 'number') {
        const decimalPlaces = (value.toString().split('.')[1] || '').length;
        if (decimalPlaces > rules.decimals) {
          errors.push(`Maximum ${rules.decimals} decimal places allowed`);
        }
      }
    }
    
    return errors;
  }

  /**
   * Data converters for COBOL integration
   */

  /**
   * Convert TypeScript object to COBOL format
   */
  export function toCobolFormat<T extends keyof typeof validators>(
    recordType: T,
    data: any
  ): Record<string, any> {
    const result: Record<string, any> = {};
    const structure = validators[recordType];

    for (const [field, rules] of Object.entries(structure)) {
      const value = data[field];
      if (value !== undefined) {
        result[field] = formatForCobol(value, rules as any);
      }
    }

    return result;
  }

  /**
   * Convert COBOL data to TypeScript format
   */
  export function fromCobolFormat<T extends keyof typeof validators>(
    recordType: T,
    data: any
  ): any {
    const result: any = {};
    const structure = validators[recordType];

    for (const [field, rules] of Object.entries(structure)) {
      const value = data[field];
      if (value !== undefined) {
        result[field] = parseFromCobol(value, rules as any);
      }
    }

    return result;
  }

  function formatForCobol(value: any, rules: any): any {
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
        return parseFloat(`${intPart}.${decPart}`);
      }
      return parseInt(str) || 0;
    } else if (rules.type === 'string') {
      return String(value).trimEnd();
    }
    return value;
  }

  /**
   * Export all interfaces for external use
   */
  export type RecordTypes = keyof typeof validators;
}

// Re-export for convenience
export const CobolTypesValidators = CobolTypes.validators;
export const CobolTypesValidate = CobolTypes.validate;
export const CobolTypesToCobol = CobolTypes.toCobolFormat;
export const CobolTypesFromCobol = CobolTypes.fromCobolFormat;