<?php
/**
 * Factory for creating SuiteCRM-specific mocks and test doubles
 */

class SugarMockFactory
{
    /**
     * Create a mock SugarBean with common properties and methods
     */
    public static function createMockBean($beanType = 'Generic', $id = null)
    {
        $mock = new class {
            public $id;
            public $module_name;
            public $table_name;
            public $in_import = false;
            public $new_with_id = false;
            public $process_save_dates = true;
            
            protected $relationships = [];
            protected $field_defs = [];
            
            public function __construct() {
                $this->id = $this->id ?: self::createGuid();
            }
            
            public function load_relationship($link_name) {
                return isset($this->relationships[$link_name]);
            }
            
            public function save($check_notify = false) {
                return $this->id;
            }
            
            public function retrieve($id = null) {
                if ($id) {
                    $this->id = $id;
                }
                return $this;
            }
            
            public function mark_deleted($id) {
                $this->deleted = 1;
                return true;
            }
            
            public function get_field_def($field_name) {
                return $this->field_defs[$field_name] ?? null;
            }
            
            public function get_custom_table_name() {
                return $this->table_name . '_cstm';
            }
            
            private static function createGuid() {
                return sprintf('%04x%04x-%04x-%04x-%04x-%04x%04x%04x',
                    mt_rand(0, 0xffff), mt_rand(0, 0xffff),
                    mt_rand(0, 0xffff),
                    mt_rand(0, 0x0fff) | 0x4000,
                    mt_rand(0, 0x3fff) | 0x8000,
                    mt_rand(0, 0xffff), mt_rand(0, 0xffff), mt_rand(0, 0xffff)
                );
            }
        };
        
        $mock->module_name = $beanType;
        $mock->table_name = strtolower($beanType);
        
        if ($id) {
            $mock->id = $id;
        }
        
        return $mock;
    }
    
    /**
     * Create a mock Quote bean with specific properties
     */
    public static function createMockQuote($id = null)
    {
        $quote = self::createMockBean('AOS_Quotes', $id);
        
        // Add quote-specific properties
        $quote->name = 'Test Quote';
        $quote->billing_account_id = 'account-' . substr(md5(rand()), 0, 8);
        $quote->currency_id = 'USD';
        $quote->subtotal_amount = 0;
        $quote->discount_amount = 0;
        $quote->tax_amount = 0;
        $quote->shipping_amount = 0;
        $quote->total_amount = 0;
        $quote->description = '';
        
        // Custom fields
        $quote->cobol_calculated_c = 0;
        $quote->cobol_execution_time_c = 0;
        
        // Mock line items relationship
        $quote->relationships['aos_products_quotes'] = new class {
            private $lineItems = [];
            
            public function getBeans() {
                return $this->lineItems;
            }
            
            public function addLineItem($item) {
                $this->lineItems[] = $item;
            }
        };
        
        return $quote;
    }
    
    /**
     * Create a mock line item for quotes
     */
    public static function createMockQuoteLineItem($productId = null)
    {
        $lineItem = self::createMockBean('AOS_Products_Quotes');
        
        $lineItem->product_id = $productId ?: 'product-' . substr(md5(rand()), 0, 8);
        $lineItem->product_qty = '1';
        $lineItem->product_unit_price = '100.00';
        $lineItem->product_discount = '0';
        $lineItem->product_discount_type = 'amount';
        $lineItem->vat = '0';
        $lineItem->product_total_price = '100.00';
        $lineItem->vat_amt = '0';
        
        return $lineItem;
    }
    
    /**
     * Create a mock User
     */
    public static function createMockUser($id = null)
    {
        $user = self::createMockBean('Users', $id);
        
        $user->user_name = 'testuser';
        $user->first_name = 'Test';
        $user->last_name = 'User';
        $user->email1 = 'test@example.com';
        $user->status = 'Active';
        $user->is_admin = 0;
        
        return $user;
    }
    
    /**
     * Create a mock Database connection
     */
    public static function createMockDatabase()
    {
        return new class {
            public $queries = [];
            public $results = [];
            
            public function query($sql, $dieOnError = false, $msg = '') {
                $this->queries[] = $sql;
                
                // Return mock result based on query type
                if (stripos($sql, 'SELECT') === 0) {
                    return $this->createMockResult();
                } elseif (stripos($sql, 'INSERT') === 0) {
                    return true;
                } elseif (stripos($sql, 'UPDATE') === 0) {
                    return true;
                } elseif (stripos($sql, 'DELETE') === 0) {
                    return true;
                } elseif (stripos($sql, 'CREATE') === 0) {
                    return true;
                }
                
                return true;
            }
            
            public function fetchRow($result) {
                return false; // No more rows
            }
            
            public function quote($string) {
                return "'" . addslashes($string) . "'";
            }
            
            public function getAffectedRowCount($result) {
                return 1;
            }
            
            public function disconnect() {
                return true;
            }
            
            private function createMockResult() {
                return new class {
                    public function fetchRow() {
                        return false;
                    }
                };
            }
        };
    }
    
    /**
     * Create a mock Logger
     */
    public static function createMockLogger()
    {
        return new class {
            public $logs = [];
            
            public function debug($message, $context = []) {
                $this->logs[] = ['level' => 'debug', 'message' => $message, 'context' => $context];
            }
            
            public function info($message, $context = []) {
                $this->logs[] = ['level' => 'info', 'message' => $message, 'context' => $context];
            }
            
            public function warn($message, $context = []) {
                $this->logs[] = ['level' => 'warn', 'message' => $message, 'context' => $context];
            }
            
            public function error($message, $context = []) {
                $this->logs[] = ['level' => 'error', 'message' => $message, 'context' => $context];
            }
            
            public function fatal($message, $context = []) {
                $this->logs[] = ['level' => 'fatal', 'message' => $message, 'context' => $context];
            }
            
            public function getLogs($level = null) {
                if ($level) {
                    return array_filter($this->logs, function($log) use ($level) {
                        return $log['level'] === $level;
                    });
                }
                return $this->logs;
            }
            
            public function clearLogs() {
                $this->logs = [];
            }
        };
    }
    
    /**
     * Create a mock HTTP response for API calls
     */
    public static function createMockHttpResponse($statusCode = 200, $body = '', $headers = [])
    {
        return new class($statusCode, $body, $headers) {
            private $statusCode;
            private $body;
            private $headers;
            
            public function __construct($statusCode, $body, $headers) {
                $this->statusCode = $statusCode;
                $this->body = $body;
                $this->headers = $headers;
            }
            
            public function getStatusCode() {
                return $this->statusCode;
            }
            
            public function getBody() {
                return $this->body;
            }
            
            public function getHeaders() {
                return $this->headers;
            }
            
            public function getHeader($name) {
                return $this->headers[$name] ?? null;
            }
        };
    }
    
    /**
     * Create a mock COBOL API response
     */
    public static function createMockCobolResponse($success = true, $data = null, $error = null)
    {
        $response = [
            'success' => $success,
            'timestamp' => date('c')
        ];
        
        if ($success && $data) {
            $response['data'] = $data;
        }
        
        if (!$success && $error) {
            $response['error'] = $error;
        }
        
        return $response;
    }
    
    /**
     * Set up common test environment
     */
    public static function setupTestEnvironment()
    {
        // Mock current user
        $GLOBALS['current_user'] = self::createMockUser('test-user-123');
        
        // Mock database
        $GLOBALS['db'] = self::createMockDatabase();
        
        // Mock logger
        $GLOBALS['log'] = self::createMockLogger();
        
        // Mock app strings
        $GLOBALS['app_strings'] = [
            'LBL_SAVE' => 'Save',
            'LBL_CANCEL' => 'Cancel',
            'LBL_DELETE' => 'Delete',
            'LBL_EDIT' => 'Edit'
        ];
        
        // Mock module strings
        $GLOBALS['mod_strings'] = [
            'LBL_NAME' => 'Name',
            'LBL_DESCRIPTION' => 'Description'
        ];
        
        return [
            'user' => $GLOBALS['current_user'],
            'db' => $GLOBALS['db'],
            'log' => $GLOBALS['log']
        ];
    }
    
    /**
     * Clean up test environment
     */
    public static function tearDownTestEnvironment()
    {
        unset($GLOBALS['current_user']);
        unset($GLOBALS['db']);
        unset($GLOBALS['log']);
        unset($GLOBALS['app_strings']);
        unset($GLOBALS['mod_strings']);
    }
    
    /**
     * Create a temporary file for testing
     */
    public static function createTempFile($content = '', $extension = '.tmp')
    {
        $tempFile = tempnam(sys_get_temp_dir(), 'suitecrm_test_') . $extension;
        file_put_contents($tempFile, $content);
        
        // Register for cleanup
        register_shutdown_function(function() use ($tempFile) {
            if (file_exists($tempFile)) {
                unlink($tempFile);
            }
        });
        
        return $tempFile;
    }
    
    /**
     * Create test data fixtures
     */
    public static function createTestFixtures()
    {
        return [
            'quote_calculation_params' => [
                'quote_id' => 'quote-123-456',
                'customer_id' => 'account-789',
                'currency' => 'USD',
                'items' => [
                    [
                        'product_id' => 'product-001',
                        'quantity' => 2.0,
                        'unit_price' => 199.99,
                        'discount' => 10.0,
                        'discount_type' => 'percentage',
                        'tax_rate' => 8.5
                    ]
                ],
                'discount' => 50.0,
                'shipping' => 25.0,
                'tax_rate' => 8.5
            ],
            
            'cobol_response_success' => [
                'success' => true,
                'data' => [
                    'subtotal' => 359.98,
                    'discount_amount' => 50.0,
                    'tax_amount' => 26.35,
                    'shipping_amount' => 25.0,
                    'total_amount' => 361.33,
                    'line_items' => [
                        [
                            'total_price' => 359.98,
                            'vat_amount' => 26.35
                        ]
                    ],
                    'notes' => 'Calculated using enhanced financial algorithm'
                ]
            ],
            
            'cobol_response_error' => [
                'success' => false,
                'error' => 'COBOL service temporarily unavailable',
                'code' => 503
            ]
        ];
    }
}
?>