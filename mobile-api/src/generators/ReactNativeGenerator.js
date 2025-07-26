const winston = require('winston');

class ReactNativeGenerator {
  constructor() {
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });
  }

  /**
   * Generate React Native component from COBOL screen definition
   * @param {string} cobolScreen - COBOL screen definition
   * @param {string} screenName - Name for the generated component
   * @param {object} options - Generation options
   * @returns {object} Generated component code and metadata
   */
  async generateComponent(cobolScreen, screenName, options = {}) {
    try {
      const fields = this.parseCobolScreen(cobolScreen);
      const componentName = this.sanitizeComponentName(screenName);
      
      const component = this.generateReactNativeComponent(componentName, fields, options);
      const styles = this.generateStyles(fields, options);
      const validation = this.generateValidation(fields);
      
      return {
        component,
        styles,
        validation,
        fields: fields.map(f => ({
          name: f.name,
          type: f.type,
          label: f.label,
          required: f.required,
          picClause: f.picClause
        }))
      };
    } catch (error) {
      this.logger.error('Error generating React Native component:', error);
      throw error;
    }
  }

  /**
   * Parse COBOL screen definition into field objects
   */
  parseCobolScreen(cobolScreen) {
    const fields = [];
    const lines = cobolScreen.split('\n');
    
    lines.forEach(line => {
      // Parse COBOL screen definition lines
      // Example: 05 CUSTOMER-NAME PIC X(30) VALUE SPACES.
      // Example: 05 ACCOUNT-NUMBER PIC 9(10) VALUE ZEROS.
      // Example: 05 BALANCE PIC S9(7)V99 VALUE ZEROS.
      
      const fieldMatch = line.match(/^\s*(\d+)\s+([A-Z0-9-]+)\s+PIC\s+([^\s]+)(?:\s+VALUE\s+([^\s.]+))?/);
      
      if (fieldMatch) {
        const [, level, name, picClause, defaultValue] = fieldMatch;
        
        const field = {
          level: parseInt(level),
          name: this.camelCase(name),
          originalName: name,
          picClause,
          defaultValue: this.parseDefaultValue(defaultValue),
          type: this.getFieldType(picClause),
          maxLength: this.getMaxLength(picClause),
          required: !defaultValue || defaultValue === 'SPACES' || defaultValue === 'ZEROS',
          label: this.generateLabel(name)
        };
        
        // Handle display attributes
        if (line.includes('HIGHLIGHT')) field.highlight = true;
        if (line.includes('REVERSE-VIDEO')) field.reverseVideo = true;
        if (line.includes('UNDERLINE')) field.underline = true;
        if (line.includes('BLINK')) field.blink = true;
        
        fields.push(field);
      }
    });
    
    return fields;
  }

  /**
   * Generate React Native component code
   */
  generateReactNativeComponent(componentName, fields, options = {}) {
    const imports = this.generateImports(fields, options);
    const state = this.generateState(fields);
    const handlers = this.generateHandlers(fields);
    const renderFields = this.generateRenderFields(fields);
    
    return `${imports}

interface ${componentName}Props {
  navigation: any;
  route: any;
  onSubmit?: (data: any) => void;
}

interface ${componentName}State {
${state}
  errors: Record<string, string>;
  loading: boolean;
}

export default class ${componentName} extends React.Component<${componentName}Props, ${componentName}State> {
  constructor(props: ${componentName}Props) {
    super(props);
    
    this.state = {
${this.generateInitialState(fields)}
      errors: {},
      loading: false
    };
  }

  componentDidMount() {
    // Load any initial data
    this.loadData();
  }

  loadData = async () => {
    try {
      this.setState({ loading: true });
      // Load data from API or offline storage
      const data = await OfflineStorage.get('${componentName.toLowerCase()}_data');
      if (data) {
        this.setState(data);
      }
    } catch (error) {
      Alert.alert('Error', 'Failed to load data');
    } finally {
      this.setState({ loading: false });
    }
  };

${handlers}

  validate = (): boolean => {
    const errors: Record<string, string> = {};
    
${this.generateValidationLogic(fields)}
    
    this.setState({ errors });
    return Object.keys(errors).length === 0;
  };

  handleSubmit = async () => {
    if (!this.validate()) {
      Alert.alert('Validation Error', 'Please fix the errors before submitting');
      return;
    }

    try {
      this.setState({ loading: true });
      
      const data = {
${fields.map(f => `        ${f.name}: this.state.${f.name},`).join('\n')}
      };

      // Save to offline storage
      await OfflineStorage.save('${componentName.toLowerCase()}_data', data);
      
      // Try to sync with server
      if (await NetworkUtils.isConnected()) {
        await ApiClient.post('/api/cobol/${componentName.toLowerCase()}', data);
      } else {
        // Queue for later sync
        await SyncQueue.add({
          type: 'CREATE',
          entity: '${componentName.toLowerCase()}',
          data,
          timestamp: Date.now()
        });
      }

      if (this.props.onSubmit) {
        this.props.onSubmit(data);
      } else {
        this.props.navigation.goBack();
      }
    } catch (error) {
      Alert.alert('Error', 'Failed to save data');
    } finally {
      this.setState({ loading: false });
    }
  };

  render() {
    const { errors, loading } = this.state;

    return (
      <SafeAreaView style={styles.container}>
        <KeyboardAvoidingView
          behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
          style={styles.keyboardView}
        >
          <ScrollView 
            style={styles.scrollView}
            contentContainerStyle={styles.scrollContent}
            keyboardShouldPersistTaps="handled"
          >
            <View style={styles.header}>
              <Text style={styles.title}>${this.generateLabel(componentName)}</Text>
            </View>

${renderFields}

            <View style={styles.buttonContainer}>
              <TouchableOpacity
                style={[styles.button, styles.submitButton]}
                onPress={this.handleSubmit}
                disabled={loading}
              >
                {loading ? (
                  <ActivityIndicator color="#FFFFFF" />
                ) : (
                  <Text style={styles.buttonText}>Submit</Text>
                )}
              </TouchableOpacity>
              
              <TouchableOpacity
                style={[styles.button, styles.cancelButton]}
                onPress={() => this.props.navigation.goBack()}
                disabled={loading}
              >
                <Text style={styles.buttonText}>Cancel</Text>
              </TouchableOpacity>
            </View>
          </ScrollView>
        </KeyboardAvoidingView>
      </SafeAreaView>
    );
  }
}`;
  }

  /**
   * Generate imports based on fields
   */
  generateImports(fields, options) {
    const hasDateField = fields.some(f => f.type === 'date');
    const hasPickerField = fields.some(f => f.type === 'picker');
    
    let imports = `import React from 'react';
import {
  View,
  Text,
  TextInput,
  TouchableOpacity,
  ScrollView,
  KeyboardAvoidingView,
  Platform,
  Alert,
  ActivityIndicator,
  SafeAreaView,
} from 'react-native';
import { styles } from './styles';
import { OfflineStorage, NetworkUtils, ApiClient, SyncQueue } from '../services';
import { validateField } from '../validators';`;

    if (hasDateField) {
      imports += `\nimport DateTimePicker from '@react-native-community/datetimepicker';`;
    }
    
    if (hasPickerField) {
      imports += `\nimport { Picker } from '@react-native-picker/picker';`;
    }

    return imports;
  }

  /**
   * Generate state interface
   */
  generateState(fields) {
    return fields.map(field => {
      let type = 'string';
      if (field.type === 'number') type = 'number';
      if (field.type === 'boolean') type = 'boolean';
      if (field.type === 'date') type = 'Date | null';
      
      return `  ${field.name}: ${type};`;
    }).join('\n');
  }

  /**
   * Generate initial state
   */
  generateInitialState(fields) {
    return fields.map(field => {
      let value = "''";
      if (field.type === 'number') value = '0';
      if (field.type === 'boolean') value = 'false';
      if (field.type === 'date') value = 'null';
      if (field.defaultValue !== undefined) {
        value = typeof field.defaultValue === 'string' 
          ? `'${field.defaultValue}'` 
          : field.defaultValue;
      }
      
      return `      ${field.name}: ${value},`;
    }).join('\n');
  }

  /**
   * Generate field handlers
   */
  generateHandlers(fields) {
    const handlers = fields.map(field => {
      if (field.type === 'date') {
        return `  handle${this.capitalize(field.name)}Change = (event: any, selectedDate?: Date) => {
    const currentDate = selectedDate || this.state.${field.name};
    this.setState({ ${field.name}: currentDate });
  };`;
      } else {
        return `  handle${this.capitalize(field.name)}Change = (value: ${field.type === 'number' ? 'string' : 'any'}) => {
    ${field.type === 'number' ? `const numValue = parseFloat(value) || 0;
    this.setState({ ${field.name}: numValue });` : `this.setState({ ${field.name}: value });`}
    
    // Clear error when user starts typing
    if (this.state.errors.${field.name}) {
      const errors = { ...this.state.errors };
      delete errors.${field.name};
      this.setState({ errors });
    }
  };`;
      }
    }).join('\n\n');
    
    return handlers;
  }

  /**
   * Generate render fields
   */
  generateRenderFields(fields) {
    return fields.map(field => {
      const errorCheck = `{errors.${field.name} && (
              <Text style={styles.errorText}>{errors.${field.name}}</Text>
            )}`;

      if (field.type === 'boolean') {
        return `            <View style={styles.fieldContainer}>
              <Text style={styles.label}>${field.label}</Text>
              <TouchableOpacity
                style={[styles.checkbox, this.state.${field.name} && styles.checkboxChecked]}
                onPress={() => this.handle${this.capitalize(field.name)}Change(!this.state.${field.name})}
              >
                {this.state.${field.name} && <Text style={styles.checkmark}>✓</Text>}
              </TouchableOpacity>
            </View>`;
      }

      if (field.type === 'date') {
        return `            <View style={styles.fieldContainer}>
              <Text style={styles.label}>${field.label}</Text>
              <TouchableOpacity
                style={styles.dateButton}
                onPress={() => this.setState({ show${this.capitalize(field.name)}Picker: true })}
              >
                <Text>{this.state.${field.name} ? this.state.${field.name}.toLocaleDateString() : 'Select date'}</Text>
              </TouchableOpacity>
              ${errorCheck}
              {this.state.show${this.capitalize(field.name)}Picker && (
                <DateTimePicker
                  value={this.state.${field.name} || new Date()}
                  mode="date"
                  display="default"
                  onChange={this.handle${this.capitalize(field.name)}Change}
                />
              )}
            </View>`;
      }

      // Default text input
      return `            <View style={styles.fieldContainer}>
              <Text style={styles.label}>${field.label}${field.required ? ' *' : ''}</Text>
              <TextInput
                style={[styles.input, errors.${field.name} && styles.inputError]}
                value={String(this.state.${field.name})}
                onChangeText={this.handle${this.capitalize(field.name)}Change}
                placeholder="Enter ${field.label.toLowerCase()}"
                ${field.type === 'number' ? 'keyboardType="numeric"' : ''}
                ${field.maxLength ? `maxLength={${field.maxLength}}` : ''}
                ${field.type === 'password' ? 'secureTextEntry' : ''}
              />
              ${errorCheck}
            </View>`;
    }).join('\n\n');
  }

  /**
   * Generate validation logic
   */
  generateValidationLogic(fields) {
    return fields.filter(f => f.required).map(field => {
      return `    if (!this.state.${field.name}) {
      errors.${field.name} = '${field.label} is required';
    } else {
      const validation = validateField(this.state.${field.name}, '${field.picClause}', '${field.name}');
      if (!validation.isValid) {
        errors.${field.name} = validation.error;
      }
    }`;
    }).join('\n\n');
  }

  /**
   * Generate styles
   */
  generateStyles(fields, options) {
    return `import { StyleSheet } from 'react-native';

export const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#F5F5F5',
  },
  keyboardView: {
    flex: 1,
  },
  scrollView: {
    flex: 1,
  },
  scrollContent: {
    padding: 20,
  },
  header: {
    marginBottom: 30,
  },
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    color: '#333',
    textAlign: 'center',
  },
  fieldContainer: {
    marginBottom: 20,
  },
  label: {
    fontSize: 16,
    fontWeight: '600',
    color: '#666',
    marginBottom: 8,
  },
  input: {
    borderWidth: 1,
    borderColor: '#DDD',
    borderRadius: 8,
    padding: 12,
    fontSize: 16,
    backgroundColor: '#FFF',
  },
  inputError: {
    borderColor: '#FF3B30',
  },
  errorText: {
    color: '#FF3B30',
    fontSize: 14,
    marginTop: 4,
  },
  buttonContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    marginTop: 30,
  },
  button: {
    flex: 1,
    padding: 16,
    borderRadius: 8,
    alignItems: 'center',
    marginHorizontal: 8,
  },
  submitButton: {
    backgroundColor: '#007AFF',
  },
  cancelButton: {
    backgroundColor: '#8E8E93',
  },
  buttonText: {
    color: '#FFFFFF',
    fontSize: 18,
    fontWeight: '600',
  },
  checkbox: {
    width: 24,
    height: 24,
    borderWidth: 2,
    borderColor: '#007AFF',
    borderRadius: 4,
    alignItems: 'center',
    justifyContent: 'center',
  },
  checkboxChecked: {
    backgroundColor: '#007AFF',
  },
  checkmark: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: 'bold',
  },
  dateButton: {
    borderWidth: 1,
    borderColor: '#DDD',
    borderRadius: 8,
    padding: 12,
    backgroundColor: '#FFF',
  },
});`;
  }

  /**
   * Generate validation rules
   */
  generateValidation(fields) {
    const rules = {};
    
    fields.forEach(field => {
      const rule = {
        required: field.required,
        picClause: field.picClause,
        type: field.type,
        maxLength: field.maxLength
      };
      
      if (field.type === 'number') {
        const { min, max, decimals } = this.getNumericConstraints(field.picClause);
        if (min !== undefined) rule.min = min;
        if (max !== undefined) rule.max = max;
        if (decimals !== undefined) rule.decimals = decimals;
      }
      
      rules[field.name] = rule;
    });
    
    return rules;
  }

  /**
   * Helper methods
   */
  sanitizeComponentName(name) {
    return name.replace(/[^a-zA-Z0-9]/g, '').replace(/^[0-9]/, 'C$&');
  }

  camelCase(str) {
    return str.toLowerCase().replace(/-([a-z])/g, (g) => g[1].toUpperCase());
  }

  capitalize(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  generateLabel(name) {
    return name.replace(/[-_]/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
  }

  getFieldType(picClause) {
    if (picClause.includes('9')) return 'number';
    if (picClause.includes('X')) return 'text';
    if (picClause.includes('A')) return 'text';
    if (picClause.includes('V')) return 'number';
    if (picClause.includes('S')) return 'number';
    return 'text';
  }

  getMaxLength(picClause) {
    const match = picClause.match(/\((\d+)\)/);
    return match ? parseInt(match[1]) : null;
  }

  parseDefaultValue(value) {
    if (!value) return undefined;
    if (value === 'SPACES') return '';
    if (value === 'ZEROS' || value === 'ZERO') return 0;
    if (value.startsWith('"') && value.endsWith('"')) {
      return value.slice(1, -1);
    }
    return value;
  }

  getNumericConstraints(picClause) {
    const constraints = {};
    
    // Extract total digits and decimals
    const match = picClause.match(/S?9\((\d+)\)(?:V9\((\d+)\))?/);
    if (match) {
      const [, integerDigits, decimalDigits] = match;
      const totalDigits = parseInt(integerDigits) + (decimalDigits ? parseInt(decimalDigits) : 0);
      
      constraints.max = Math.pow(10, parseInt(integerDigits)) - 1;
      constraints.min = picClause.includes('S') ? -constraints.max : 0;
      constraints.decimals = decimalDigits ? parseInt(decimalDigits) : 0;
    }
    
    return constraints;
  }
}

module.exports = ReactNativeGenerator;