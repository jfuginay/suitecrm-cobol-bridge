# SuiteCRM-COBOL Bridge Mobile API

A comprehensive mobile API that bridges COBOL mainframe systems with modern React Native applications, providing seamless integration with automatic component generation, data conversion, and offline synchronization.

## Features

### 🚀 Core Capabilities

- **Auto-generate React Native Components** from COBOL screen definitions
- **EBCDIC to UTF-8 Conversion** for mainframe data compatibility
- **PIC Clause Validation** enforcing COBOL data rules on mobile
- **Offline Support** with intelligent sync queue management
- **TypeScript Generation** from COBOL COPYBOOK definitions
- **Real-time Push Notifications** via WebSocket and Firebase/Expo

### 📱 Mobile-First Design

- Optimized for React Native applications
- Built-in offline queue management
- Automatic retry mechanisms
- Conflict resolution strategies
- Device-specific push notifications

## Installation

```bash
cd mobile-api
npm install

# Configure environment
cp .env.example .env
# Edit .env with your configuration

# Start the server
npm start
```

## Configuration

### Environment Variables

```env
# Server
PORT=3002
NODE_ENV=production

# Authentication
JWT_SECRET=your-secure-jwt-secret

# Mobile App
MOBILE_APP_URL=http://localhost:19000

# Firebase (optional)
FIREBASE_SERVICE_ACCOUNT_PATH=/path/to/serviceAccount.json

# COBOL Bridge
COBOL_API_URL=http://localhost:3001
```

## API Endpoints

### Component Generation

```javascript
POST /api/generate/components
{
  "cobolScreen": "01 CUSTOMER-SCREEN.\n   05 CUSTOMER-NAME PIC X(30)...",
  "screenName": "CustomerForm",
  "options": {
    "includeValidation": true,
    "includeStyles": true
  }
}
```

### TypeScript Generation

```javascript
POST /api/generate/types
{
  "copybook": "01 CUSTOMER-RECORD.\n   05 CUST-ID PIC 9(10)...",
  "namespace": "CustomerTypes"
}
```

### EBCDIC Conversion

```javascript
POST /api/convert/ebcdic
{
  "data": "F1F2F3",  // EBCDIC hex string
  "encoding": "IBM037"
}
```

### Field Validation

```javascript
POST /api/validate/field
{
  "value": "12345",
  "picClause": "9(5)",
  "fieldName": "zipCode"
}
```

### Offline Sync

```javascript
POST /api/sync/upload
{
  "transactions": [
    {
      "entityType": "customer",
      "operation": "CREATE",
      "data": { ... }
    }
  ],
  "deviceId": "device-123"
}
```

### Push Notifications

```javascript
POST /api/push/register
{
  "deviceToken": "ExponentPushToken[...]",
  "platform": "expo",
  "deviceId": "device-123"
}
```

## React Native Integration

### Sample Implementation

```javascript
import { MobileAPIClient } from './MobileAPIClient';

// Initialize client
const client = new MobileAPIClient('http://localhost:3002', authToken);
await client.initialize();

// Generate component from COBOL screen
const component = await client.generateComponent(
  cobolScreenDefinition,
  'CustomerForm'
);

// Validate field
const validation = await client.validateField(
  '12345',
  '9(5)',
  'zipCode'
);

// Handle offline sync
await client.queueForSync({
  entityType: 'customer',
  operation: 'UPDATE',
  data: customerData
});
```

### Generated Component Example

The API automatically generates React Native components from COBOL screens:

```javascript
// Generated from COBOL screen definition
export default class CustomerForm extends React.Component {
  state = {
    customerName: '',
    accountNumber: 0,
    balance: 0,
    errors: {},
    loading: false
  };

  validate = () => {
    // Auto-generated validation based on PIC clauses
    const errors = {};
    
    if (!this.state.customerName) {
      errors.customerName = 'Customer Name is required';
    }
    
    const validation = validateField(
      this.state.accountNumber,
      '9(10)',
      'accountNumber'
    );
    
    if (!validation.isValid) {
      errors.accountNumber = validation.error;
    }
    
    return Object.keys(errors).length === 0;
  };
  
  // ... rest of component
}
```

## TypeScript Support

### Generated Types from COPYBOOK

```typescript
export namespace CustomerTypes {
  export interface CustomerRecord {
    custId: number;        // PIC 9(10)
    custName: string;      // PIC X(30)
    custAddress: {
      street: string;      // PIC X(30)
      city: string;        // PIC X(20)
      state: string;       // PIC X(2)
      zip: number;         // PIC 9(5)
    };
    custBalance: number;   // PIC S9(7)V99 COMP-3
  }
  
  // Auto-generated validators
  export const validators = { ... };
  
  // Conversion functions
  export function toCobolFormat(data: CustomerRecord): any;
  export function fromCobolFormat(data: any): CustomerRecord;
}
```

## Offline Sync Architecture

### Sync Queue Management

1. **Automatic Queuing**: Failed requests are automatically queued
2. **Batch Processing**: Multiple transactions synced together
3. **Conflict Resolution**: Server-side version checking
4. **Retry Logic**: Exponential backoff for failed syncs

### Sync Flow

```
Mobile App → API Request
    ↓ (if offline)
Local Queue → Background Sync
    ↓ (when online)
Batch Upload → Server Processing
    ↓
Real-time Updates → All Devices
```

## WebSocket Events

### Client Events

- `authenticate`: Authenticate socket connection
- `subscribe:updates`: Subscribe to entity updates
- `sync:request`: Request pending updates

### Server Events

- `authenticated`: Authentication successful
- `sync:update`: Real-time data update
- `entity:update`: Entity-specific update
- `sync:error`: Sync error notification

## Best Practices

### 1. Component Generation

- Keep COBOL screens well-structured
- Use meaningful field names
- Include appropriate PIC clauses
- Add VALUE clauses for defaults

### 2. Offline Support

- Design for offline-first
- Handle sync conflicts gracefully
- Implement proper error handling
- Test offline scenarios

### 3. Performance

- Batch sync operations
- Use WebSocket for real-time updates
- Implement proper caching
- Minimize payload sizes

### 4. Security

- Always use HTTPS in production
- Implement proper authentication
- Validate all inputs
- Sanitize COBOL data

## Testing

```bash
# Run tests
npm test

# Test component generation
npm run test:generate

# Test offline sync
npm run test:sync
```

## Monitoring

The API provides health checks and metrics:

```bash
GET /health
{
  "status": "healthy",
  "services": {
    "database": true,
    "push": true
  }
}
```

## Troubleshooting

### Common Issues

1. **EBCDIC Conversion Errors**
   - Verify encoding type (IBM037, IBM1047, etc.)
   - Check data format (hex string vs base64)

2. **Sync Failures**
   - Check network connectivity
   - Verify authentication token
   - Review conflict resolution logs

3. **Push Notification Issues**
   - Confirm device registration
   - Check Firebase/Expo configuration
   - Verify notification permissions

## Support

For issues and feature requests, please refer to the main project documentation or contact the development team.

## License

This project is part of the SuiteCRM-COBOL Bridge system. See LICENSE for details.