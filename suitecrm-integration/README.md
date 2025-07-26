# SuiteCRM COBOL Bridge Integration

This module provides complete integration between SuiteCRM and COBOL programs through the COBOL API Gateway.

## Features

### 1. COBOL Programs Module
- List and manage COBOL programs
- View execution history
- Execute programs directly from SuiteCRM
- Real-time monitoring of program executions
- Track success rates and performance metrics

### 2. COBOL Rules Module
- Display business rules extracted from COBOL
- Visual rule editor integration
- Generate COBOL code from edited rules
- Test case management
- Rule validation

### 3. COBOL Jobs Module
- Track batch job executions
- Real-time progress monitoring
- Pause/resume functionality
- Job scheduling and retry management
- Parent-child job relationships

### 4. Additional Features
- Quote calculations using COBOL financial programs
- Real-time monitoring dashlets
- WebSocket integration for live updates
- Comprehensive administration panel
- Execution history tracking

## Installation

1. Ensure the COBOL API Gateway is running on port 3000
2. Navigate to Module Loader in SuiteCRM Admin
3. Upload the module package
4. Install and execute post-install scripts
5. Configure API settings in Admin > COBOL Bridge > API Configuration

## Configuration

### API Settings
- **COBOL API URL**: Default `http://localhost:3000/api/v1`
- **COBOL API Key**: Your API authentication key
- **WebSocket Enabled**: Enable real-time updates
- **Batch Job Timeout**: Maximum execution time for batch jobs

### Quote Integration
The module automatically integrates with AOS_Quotes to perform financial calculations using COBOL programs.

## Module Structure

```
suitecrm-integration/
├── manifest.php                 # Module manifest
├── modules/
│   ├── COBOL_Programs/         # Program management module
│   ├── COBOL_Rules/            # Rule management module
│   └── COBOL_Jobs/             # Job tracking module
├── custom/
│   └── include/CobolBridge/    # Core integration logic
├── language/                   # Language files
├── scripts/                    # Install/uninstall scripts
└── samples/                    # Sample COBOL programs
```

## Usage

### Managing COBOL Programs
1. Navigate to COBOL Programs module
2. Create or import COBOL programs
3. Configure input/output parameters
4. Execute programs and view results

### Editing Business Rules
1. Navigate to COBOL Rules module
2. Select a rule to edit
3. Click "Visual Editor" to open the rule editor
4. Make changes and generate updated COBOL code

### Monitoring Batch Jobs
1. Navigate to COBOL Jobs module
2. View real-time progress of running jobs
3. Pause, resume, or cancel jobs as needed
4. Review execution logs and errors

## API Integration

The module communicates with the COBOL API Gateway for:
- Program execution
- Rule validation
- Job management
- Real-time monitoring

## Security

- All API calls use Bearer token authentication
- SSL/TLS support for secure communication
- Role-based access control through SuiteCRM ACL

## Troubleshooting

### Connection Issues
1. Verify API Gateway is running
2. Check API URL configuration
3. Validate API key
4. Review firewall settings

### Performance
1. Monitor execution times in program history
2. Check batch job logs for bottlenecks
3. Adjust timeout settings if needed

## Support

For issues or questions:
1. Check the error logs in Admin > System > View Log
2. Review the COBOL API Gateway logs
3. Verify module permissions in Admin > Role Management