import React, { useEffect, useState } from 'react';
import {
  SafeAreaView,
  ScrollView,
  View,
  Text,
  TextInput,
  TouchableOpacity,
  Alert,
  ActivityIndicator,
  StyleSheet,
  Platform,
} from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import NetInfo from '@react-native-community/netinfo';
import * as Notifications from 'expo-notifications';
import io from 'socket.io-client';

// Import generated types
import { CobolTypes } from './types';

// Mobile API client
class MobileAPIClient {
  constructor(baseURL, token) {
    this.baseURL = baseURL;
    this.token = token;
    this.socket = null;
    this.isConnected = false;
    this.syncQueue = [];
  }

  async initialize() {
    // Set up socket connection
    this.socket = io(this.baseURL, {
      auth: { token: this.token }
    });

    this.socket.on('connect', () => {
      console.log('Connected to mobile API');
      this.isConnected = true;
      this.processSyncQueue();
    });

    this.socket.on('disconnect', () => {
      console.log('Disconnected from mobile API');
      this.isConnected = false;
    });

    this.socket.on('sync:update', (data) => {
      console.log('Received sync update:', data);
      // Handle real-time updates
    });

    // Set up push notifications
    await this.setupPushNotifications();
  }

  async setupPushNotifications() {
    const { status } = await Notifications.requestPermissionsAsync();
    if (status !== 'granted') {
      console.log('Push notification permission denied');
      return;
    }

    const token = await Notifications.getExpoPushTokenAsync();
    const deviceId = await this.getDeviceId();

    // Register device
    await this.request('/api/push/register', 'POST', {
      deviceToken: token.data,
      platform: 'expo',
      deviceId
    });
  }

  async request(endpoint, method = 'GET', data = null) {
    const options = {
      method,
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.token}`
      }
    };

    if (data) {
      options.body = JSON.stringify(data);
    }

    try {
      const response = await fetch(`${this.baseURL}${endpoint}`, options);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return await response.json();
    } catch (error) {
      console.error('API request failed:', error);
      
      // Queue for offline sync if it's a write operation
      if (method !== 'GET' && data) {
        await this.queueForSync({ endpoint, method, data });
      }
      
      throw error;
    }
  }

  async queueForSync(transaction) {
    const queue = await AsyncStorage.getItem('syncQueue');
    const currentQueue = queue ? JSON.parse(queue) : [];
    currentQueue.push({
      ...transaction,
      timestamp: Date.now(),
      id: this.generateId()
    });
    await AsyncStorage.setItem('syncQueue', JSON.stringify(currentQueue));
  }

  async processSyncQueue() {
    if (!this.isConnected) return;

    const queue = await AsyncStorage.getItem('syncQueue');
    if (!queue) return;

    const transactions = JSON.parse(queue);
    if (transactions.length === 0) return;

    try {
      const deviceId = await this.getDeviceId();
      const result = await this.request('/api/sync/upload', 'POST', {
        transactions,
        deviceId
      });

      if (result.success) {
        // Clear processed transactions
        await AsyncStorage.setItem('syncQueue', JSON.stringify([]));
        console.log('Sync completed:', result);
      }
    } catch (error) {
      console.error('Sync failed:', error);
    }
  }

  async getDeviceId() {
    let deviceId = await AsyncStorage.getItem('deviceId');
    if (!deviceId) {
      deviceId = this.generateId();
      await AsyncStorage.setItem('deviceId', deviceId);
    }
    return deviceId;
  }

  generateId() {
    return Date.now().toString(36) + Math.random().toString(36).substr(2);
  }

  // COBOL integration methods
  async generateComponent(cobolScreen, screenName, options = {}) {
    return this.request('/api/generate/components', 'POST', {
      cobolScreen,
      screenName,
      options
    });
  }

  async generateTypes(copybook, namespace) {
    return this.request('/api/generate/types', 'POST', {
      copybook,
      namespace
    });
  }

  async convertEBCDIC(data, encoding = 'IBM037') {
    return this.request('/api/convert/ebcdic', 'POST', {
      data,
      encoding
    });
  }

  async validateField(value, picClause, fieldName) {
    return this.request('/api/validate/field', 'POST', {
      value,
      picClause,
      fieldName
    });
  }
}

// Sample App Component
export default function SampleApp() {
  const [client, setClient] = useState(null);
  const [loading, setLoading] = useState(true);
  const [connected, setConnected] = useState(false);
  const [formData, setFormData] = useState({
    customerName: '',
    accountNumber: '',
    balance: ''
  });

  useEffect(() => {
    initializeApp();
  }, []);

  const initializeApp = async () => {
    try {
      // Initialize API client
      const apiClient = new MobileAPIClient(
        'http://localhost:3002',
        'your-auth-token'
      );
      await apiClient.initialize();
      setClient(apiClient);

      // Monitor network connectivity
      const unsubscribe = NetInfo.addEventListener(state => {
        setConnected(state.isConnected);
        if (state.isConnected) {
          apiClient.processSyncQueue();
        }
      });

      setLoading(false);

      return () => {
        unsubscribe();
        if (apiClient.socket) {
          apiClient.socket.disconnect();
        }
      };
    } catch (error) {
      console.error('Initialization failed:', error);
      Alert.alert('Error', 'Failed to initialize app');
      setLoading(false);
    }
  };

  const handleSubmit = async () => {
    if (!client) return;

    try {
      setLoading(true);

      // Validate fields using COBOL PIC clauses
      const validations = await Promise.all([
        client.validateField(formData.customerName, 'X(30)', 'Customer Name'),
        client.validateField(formData.accountNumber, '9(10)', 'Account Number'),
        client.validateField(formData.balance, 'S9(7)V99', 'Balance')
      ]);

      const errors = validations.filter(v => !v.isValid);
      if (errors.length > 0) {
        Alert.alert('Validation Error', errors.map(e => e.error).join('\n'));
        return;
      }

      // Convert data to COBOL format
      const cobolData = CobolTypes.CustomerRecord.toCobol({
        customerName: formData.customerName,
        accountNumber: formData.accountNumber,
        balance: parseFloat(formData.balance)
      });

      // Submit data
      await client.request('/api/customer/create', 'POST', cobolData);

      Alert.alert('Success', 'Customer record created successfully');
      
      // Clear form
      setFormData({
        customerName: '',
        accountNumber: '',
        balance: ''
      });
    } catch (error) {
      Alert.alert('Error', 'Failed to submit data. Queued for offline sync.');
    } finally {
      setLoading(false);
    }
  };

  const demonstrateFeatures = async () => {
    if (!client) return;

    try {
      // 1. Generate React Native component from COBOL screen
      const cobolScreen = `
        01 CUSTOMER-SCREEN.
           05 CUSTOMER-NAME PIC X(30) VALUE SPACES.
           05 ACCOUNT-NUMBER PIC 9(10) VALUE ZEROS.
           05 BALANCE PIC S9(7)V99 VALUE ZEROS.
           05 STATUS PIC X(1) VALUE 'A'.
      `;

      const component = await client.generateComponent(
        cobolScreen,
        'CustomerForm',
        { includeValidation: true }
      );

      console.log('Generated component:', component);

      // 2. Generate TypeScript interfaces
      const copybook = `
        01 CUSTOMER-RECORD.
           05 CUST-ID PIC 9(10).
           05 CUST-NAME PIC X(30).
           05 CUST-ADDRESS.
              10 STREET PIC X(30).
              10 CITY PIC X(20).
              10 STATE PIC X(2).
              10 ZIP PIC 9(5).
           05 CUST-BALANCE PIC S9(7)V99 COMP-3.
      `;

      const types = await client.generateTypes(copybook, 'CustomerTypes');
      console.log('Generated types:', types);

      // 3. EBCDIC conversion
      const ebcdicData = 'F1F2F3'; // EBCDIC for '123'
      const converted = await client.convertEBCDIC(ebcdicData);
      console.log('Converted from EBCDIC:', converted);

      Alert.alert('Demo Complete', 'Check console for generated code');
    } catch (error) {
      Alert.alert('Demo Error', error.message);
    }
  };

  if (loading) {
    return (
      <SafeAreaView style={styles.container}>
        <View style={styles.centerContent}>
          <ActivityIndicator size="large" color="#007AFF" />
          <Text style={styles.loadingText}>Initializing...</Text>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView style={styles.container}>
      <ScrollView contentContainerStyle={styles.scrollContent}>
        <View style={styles.header}>
          <Text style={styles.title}>SuiteCRM COBOL Mobile</Text>
          <View style={[styles.statusIndicator, connected ? styles.connected : styles.disconnected]}>
            <Text style={styles.statusText}>
              {connected ? 'Online' : 'Offline'}
            </Text>
          </View>
        </View>

        <View style={styles.form}>
          <Text style={styles.sectionTitle}>Customer Information</Text>
          
          <View style={styles.fieldContainer}>
            <Text style={styles.label}>Customer Name</Text>
            <TextInput
              style={styles.input}
              value={formData.customerName}
              onChangeText={(text) => setFormData({ ...formData, customerName: text })}
              placeholder="Enter customer name"
              maxLength={30}
            />
          </View>

          <View style={styles.fieldContainer}>
            <Text style={styles.label}>Account Number</Text>
            <TextInput
              style={styles.input}
              value={formData.accountNumber}
              onChangeText={(text) => setFormData({ ...formData, accountNumber: text })}
              placeholder="Enter account number"
              keyboardType="numeric"
              maxLength={10}
            />
          </View>

          <View style={styles.fieldContainer}>
            <Text style={styles.label}>Balance</Text>
            <TextInput
              style={styles.input}
              value={formData.balance}
              onChangeText={(text) => setFormData({ ...formData, balance: text })}
              placeholder="Enter balance"
              keyboardType="decimal-pad"
            />
          </View>

          <TouchableOpacity
            style={[styles.button, styles.primaryButton]}
            onPress={handleSubmit}
            disabled={loading}
          >
            <Text style={styles.buttonText}>Submit</Text>
          </TouchableOpacity>

          <TouchableOpacity
            style={[styles.button, styles.secondaryButton]}
            onPress={demonstrateFeatures}
          >
            <Text style={[styles.buttonText, styles.secondaryButtonText]}>
              Demo Features
            </Text>
          </TouchableOpacity>
        </View>

        <View style={styles.info}>
          <Text style={styles.infoTitle}>Features:</Text>
          <Text style={styles.infoText}>• Auto-generated from COBOL screens</Text>
          <Text style={styles.infoText}>• EBCDIC/UTF-8 conversion</Text>
          <Text style={styles.infoText}>• PIC clause validation</Text>
          <Text style={styles.infoText}>• Offline support with sync</Text>
          <Text style={styles.infoText}>• Real-time push notifications</Text>
          <Text style={styles.infoText}>• TypeScript type safety</Text>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#F5F5F5',
  },
  centerContent: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  scrollContent: {
    padding: 20,
  },
  header: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: 30,
  },
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    color: '#333',
  },
  statusIndicator: {
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 15,
  },
  connected: {
    backgroundColor: '#4CAF50',
  },
  disconnected: {
    backgroundColor: '#FF5252',
  },
  statusText: {
    color: '#FFF',
    fontSize: 12,
    fontWeight: '600',
  },
  form: {
    backgroundColor: '#FFF',
    borderRadius: 10,
    padding: 20,
    marginBottom: 20,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.1,
    shadowRadius: 4,
    elevation: 3,
  },
  sectionTitle: {
    fontSize: 18,
    fontWeight: '600',
    color: '#333',
    marginBottom: 20,
  },
  fieldContainer: {
    marginBottom: 20,
  },
  label: {
    fontSize: 16,
    fontWeight: '500',
    color: '#666',
    marginBottom: 8,
  },
  input: {
    borderWidth: 1,
    borderColor: '#DDD',
    borderRadius: 8,
    padding: 12,
    fontSize: 16,
    backgroundColor: '#FAFAFA',
  },
  button: {
    padding: 16,
    borderRadius: 8,
    alignItems: 'center',
    marginTop: 10,
  },
  primaryButton: {
    backgroundColor: '#007AFF',
  },
  secondaryButton: {
    backgroundColor: '#FFF',
    borderWidth: 1,
    borderColor: '#007AFF',
  },
  buttonText: {
    color: '#FFF',
    fontSize: 18,
    fontWeight: '600',
  },
  secondaryButtonText: {
    color: '#007AFF',
  },
  loadingText: {
    marginTop: 10,
    fontSize: 16,
    color: '#666',
  },
  info: {
    backgroundColor: '#E3F2FD',
    borderRadius: 10,
    padding: 20,
  },
  infoTitle: {
    fontSize: 16,
    fontWeight: '600',
    color: '#1976D2',
    marginBottom: 10,
  },
  infoText: {
    fontSize: 14,
    color: '#1976D2',
    marginBottom: 5,
  },
});