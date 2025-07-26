const iconv = require('iconv-lite');
const winston = require('winston');

class EBCDICConverter {
  constructor() {
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });

    // Common EBCDIC encodings
    this.encodings = {
      'IBM037': 'ibm037',     // USA/Canada
      'IBM273': 'ibm273',     // Germany/Austria
      'IBM277': 'ibm277',     // Denmark/Norway
      'IBM278': 'ibm278',     // Finland/Sweden
      'IBM280': 'ibm280',     // Italy
      'IBM284': 'ibm284',     // Spain/Latin America
      'IBM285': 'ibm285',     // United Kingdom
      'IBM297': 'ibm297',     // France
      'IBM500': 'ibm500',     // International
      'IBM1047': 'ibm1047',   // Open Systems
    };

    // EBCDIC to ASCII mapping for packed decimal
    this.packedDecimalMap = {
      positive: 0xC,
      negative: 0xD,
      unsigned: 0xF
    };
  }

  /**
   * Convert EBCDIC data to UTF-8
   * @param {Buffer|string} data - EBCDIC encoded data
   * @param {string} encoding - EBCDIC encoding type
   * @returns {string} UTF-8 string
   */
  toUTF8(data, encoding = 'IBM037') {
    try {
      // Convert hex string to buffer if needed
      let buffer;
      if (typeof data === 'string') {
        // Check if it's a hex string
        if (/^[0-9A-Fa-f]+$/.test(data)) {
          buffer = Buffer.from(data, 'hex');
        } else {
          // Assume it's base64
          buffer = Buffer.from(data, 'base64');
        }
      } else {
        buffer = data;
      }

      const iconvEncoding = this.encodings[encoding] || encoding.toLowerCase();
      
      if (!iconv.encodingExists(iconvEncoding)) {
        throw new Error(`Unsupported EBCDIC encoding: ${encoding}`);
      }

      const utf8String = iconv.decode(buffer, iconvEncoding);
      
      this.logger.info(`Converted ${buffer.length} bytes from ${encoding} to UTF-8`);
      return utf8String;
    } catch (error) {
      this.logger.error('EBCDIC to UTF-8 conversion error:', error);
      throw error;
    }
  }

  /**
   * Convert UTF-8 string to EBCDIC
   * @param {string} data - UTF-8 string
   * @param {string} encoding - Target EBCDIC encoding
   * @returns {Buffer} EBCDIC encoded buffer
   */
  toEBCDIC(data, encoding = 'IBM037') {
    try {
      const iconvEncoding = this.encodings[encoding] || encoding.toLowerCase();
      
      if (!iconv.encodingExists(iconvEncoding)) {
        throw new Error(`Unsupported EBCDIC encoding: ${encoding}`);
      }

      const ebcdicBuffer = iconv.encode(data, iconvEncoding);
      
      this.logger.info(`Converted ${data.length} characters to ${encoding}`);
      return ebcdicBuffer;
    } catch (error) {
      this.logger.error('UTF-8 to EBCDIC conversion error:', error);
      throw error;
    }
  }

  /**
   * Convert EBCDIC packed decimal to number
   * @param {Buffer} buffer - Packed decimal buffer
   * @param {number} scale - Number of decimal places
   * @returns {number} Decimal number
   */
  unpackDecimal(buffer, scale = 0) {
    try {
      let value = 0;
      const bytes = Array.from(buffer);
      
      // Process all bytes except the last one
      for (let i = 0; i < bytes.length - 1; i++) {
        const byte = bytes[i];
        const digit1 = (byte >> 4) & 0x0F;
        const digit2 = byte & 0x0F;
        
        value = value * 100 + digit1 * 10 + digit2;
      }
      
      // Process the last byte (contains sign)
      const lastByte = bytes[bytes.length - 1];
      const lastDigit = (lastByte >> 4) & 0x0F;
      const sign = lastByte & 0x0F;
      
      value = value * 10 + lastDigit;
      
      // Apply sign
      if (sign === this.packedDecimalMap.negative) {
        value = -value;
      }
      
      // Apply scale
      if (scale > 0) {
        value = value / Math.pow(10, scale);
      }
      
      return value;
    } catch (error) {
      this.logger.error('Packed decimal unpacking error:', error);
      throw error;
    }
  }

  /**
   * Convert number to EBCDIC packed decimal
   * @param {number} value - Decimal number
   * @param {number} digits - Total number of digits
   * @param {number} scale - Number of decimal places
   * @returns {Buffer} Packed decimal buffer
   */
  packDecimal(value, digits, scale = 0) {
    try {
      // Handle sign
      const isNegative = value < 0;
      const absValue = Math.abs(value);
      
      // Convert to integer by removing decimals
      const intValue = Math.round(absValue * Math.pow(10, scale));
      
      // Convert to string and pad with zeros
      const strValue = intValue.toString().padStart(digits, '0');
      
      // Calculate buffer size
      const bufferSize = Math.ceil((digits + 1) / 2);
      const buffer = Buffer.alloc(bufferSize);
      
      let bufferIndex = 0;
      let nibbleHigh = true;
      
      // Pack digits
      for (let i = 0; i < strValue.length; i++) {
        const digit = parseInt(strValue[i]);
        
        if (nibbleHigh) {
          buffer[bufferIndex] = digit << 4;
          nibbleHigh = false;
        } else {
          buffer[bufferIndex] |= digit;
          bufferIndex++;
          nibbleHigh = true;
        }
      }
      
      // Set sign in the last nibble
      const sign = isNegative ? 
        this.packedDecimalMap.negative : 
        this.packedDecimalMap.positive;
      
      if (nibbleHigh) {
        buffer[bufferIndex] = sign << 4;
      } else {
        buffer[bufferIndex] |= sign;
      }
      
      return buffer;
    } catch (error) {
      this.logger.error('Packed decimal packing error:', error);
      throw error;
    }
  }

  /**
   * Convert COBOL copybook field to/from EBCDIC
   * @param {object} field - Field definition with PIC clause
   * @param {any} value - Value to convert
   * @param {string} direction - 'toEBCDIC' or 'toUTF8'
   * @param {string} encoding - EBCDIC encoding
   * @returns {any} Converted value
   */
  convertField(field, value, direction, encoding = 'IBM037') {
    try {
      const { picClause, type } = field;
      
      if (direction === 'toEBCDIC') {
        if (type === 'COMP-3' || picClause.includes('COMP-3')) {
          // Packed decimal
          const digits = this.extractDigits(picClause);
          const scale = this.extractScale(picClause);
          return this.packDecimal(value, digits, scale);
        } else if (type === 'COMP' || picClause.includes('COMP')) {
          // Binary
          return this.toBinary(value, field);
        } else {
          // Character data
          const formatted = this.formatForCobol(value, field);
          return this.toEBCDIC(formatted, encoding);
        }
      } else {
        if (type === 'COMP-3' || picClause.includes('COMP-3')) {
          // Packed decimal
          const scale = this.extractScale(picClause);
          return this.unpackDecimal(value, scale);
        } else if (type === 'COMP' || picClause.includes('COMP')) {
          // Binary
          return this.fromBinary(value, field);
        } else {
          // Character data
          const utf8 = this.toUTF8(value, encoding);
          return this.parseFromCobol(utf8, field);
        }
      }
    } catch (error) {
      this.logger.error('Field conversion error:', error);
      throw error;
    }
  }

  /**
   * Format value for COBOL field
   */
  formatForCobol(value, field) {
    const { picClause } = field;
    const length = this.extractLength(picClause);
    
    if (picClause.includes('9')) {
      // Numeric field
      const scale = this.extractScale(picClause);
      let numValue = parseFloat(value) || 0;
      
      if (scale > 0) {
        // Handle decimal
        numValue = numValue.toFixed(scale).replace('.', '');
      } else {
        numValue = Math.round(numValue).toString();
      }
      
      // Pad with zeros
      return numValue.padStart(length, '0');
    } else {
      // Character field
      return String(value).padEnd(length, ' ').substring(0, length);
    }
  }

  /**
   * Parse value from COBOL field
   */
  parseFromCobol(value, field) {
    const { picClause } = field;
    
    if (picClause.includes('9')) {
      // Numeric field
      const scale = this.extractScale(picClause);
      const trimmed = value.trim();
      
      if (scale > 0) {
        // Insert decimal point
        const intPart = trimmed.slice(0, -scale);
        const decPart = trimmed.slice(-scale);
        return parseFloat(`${intPart}.${decPart}`);
      } else {
        return parseInt(trimmed);
      }
    } else {
      // Character field - trim trailing spaces
      return value.trimEnd();
    }
  }

  /**
   * Convert to binary (COMP)
   */
  toBinary(value, field) {
    const { picClause } = field;
    const digits = this.extractDigits(picClause);
    
    // Determine byte size based on digits
    let byteSize;
    if (digits <= 4) byteSize = 2;
    else if (digits <= 9) byteSize = 4;
    else byteSize = 8;
    
    const buffer = Buffer.alloc(byteSize);
    
    if (byteSize === 2) {
      buffer.writeInt16BE(value);
    } else if (byteSize === 4) {
      buffer.writeInt32BE(value);
    } else {
      // For 8 bytes, use BigInt
      buffer.writeBigInt64BE(BigInt(value));
    }
    
    return buffer;
  }

  /**
   * Convert from binary (COMP)
   */
  fromBinary(buffer, field) {
    const byteSize = buffer.length;
    
    if (byteSize === 2) {
      return buffer.readInt16BE();
    } else if (byteSize === 4) {
      return buffer.readInt32BE();
    } else {
      return Number(buffer.readBigInt64BE());
    }
  }

  /**
   * Extract helpers
   */
  extractLength(picClause) {
    const match = picClause.match(/[9X]\((\d+)\)/);
    return match ? parseInt(match[1]) : 1;
  }

  extractDigits(picClause) {
    let total = 0;
    const matches = picClause.matchAll(/9\((\d+)\)/g);
    for (const match of matches) {
      total += parseInt(match[1]);
    }
    return total || 1;
  }

  extractScale(picClause) {
    const match = picClause.match(/V9\((\d+)\)/);
    return match ? parseInt(match[1]) : 0;
  }

  /**
   * Batch convert multiple fields
   * @param {Array} fields - Array of field definitions
   * @param {object} data - Data object with field values
   * @param {string} direction - 'toEBCDIC' or 'toUTF8'
   * @param {string} encoding - EBCDIC encoding
   * @returns {object} Converted data
   */
  convertRecord(fields, data, direction, encoding = 'IBM037') {
    const result = {};
    
    fields.forEach(field => {
      if (data.hasOwnProperty(field.name)) {
        try {
          result[field.name] = this.convertField(
            field, 
            data[field.name], 
            direction, 
            encoding
          );
        } catch (error) {
          this.logger.error(`Error converting field ${field.name}:`, error);
          result[field.name] = null;
        }
      }
    });
    
    return result;
  }
}

module.exports = EBCDICConverter;