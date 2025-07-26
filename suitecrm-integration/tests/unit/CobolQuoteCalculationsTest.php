<?php
/**
 * Unit tests for CobolQuoteCalculations class
 */

require_once('custom/include/CobolBridge/QuoteCalculations.php');

use PHPUnit\Framework\TestCase;

class CobolQuoteCalculationsTest extends TestCase
{
    protected $cobolCalculations;
    protected $mockBean;
    protected $mockDb;
    protected $mockUser;
    
    protected function setUp(): void
    {
        parent::setUp();
        
        $this->cobolCalculations = new CobolQuoteCalculations();
        
        // Mock quote bean
        $this->mockBean = $this->createMock(stdClass::class);
        $this->mockBean->id = 'quote-123-456';
        $this->mockBean->billing_account_id = 'account-789';
        $this->mockBean->currency_id = 'USD';
        $this->mockBean->discount_amount = 100.00;
        $this->mockBean->shipping_amount = 25.50;
        $this->mockBean->tax_amount = 75.00;
        $this->mockBean->description = 'Test quote';
        
        // Mock database
        $this->mockDb = $this->createMock(stdClass::class);
        $this->mockDb->method('quote')->willReturnArgument(0);
        $this->mockDb->method('query')->willReturn(true);
        
        // Mock current user
        $this->mockUser = $this->createMock(stdClass::class);
        $this->mockUser->id = 'user-123';
        
        // Set globals
        $GLOBALS['db'] = $this->mockDb;
        $GLOBALS['current_user'] = $this->mockUser;
        $GLOBALS['log'] = $this->createMock(stdClass::class);
        $GLOBALS['log']->method('error');
        
        $GLOBALS['sugar_config'] = [
            'cobol_api_url' => 'http://test-api:3000/api/v1',
            'cobol_api_key' => 'test-api-key'
        ];
    }
    
    public function testCalculateWithCOBOLSkipsWhenAlreadyCalculated()
    {
        $this->mockBean->cobol_calculated_c = 1;
        
        // Mock the prepareParameters method to ensure it's not called
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['prepareParameters'])
            ->getMock();
        
        $cobolCalculations->expects($this->never())
            ->method('prepareParameters');
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
    }
    
    public function testCalculateWithCOBOLSkipsWhenInImport()
    {
        $this->mockBean->in_import = true;
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['prepareParameters'])
            ->getMock();
        
        $cobolCalculations->expects($this->never())
            ->method('prepareParameters');
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
    }
    
    public function testPrepareParametersBasic()
    {
        // Mock the line items relationship
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('prepareParameters');
        $method->setAccessible(true);
        
        $result = $method->invoke($this->cobolCalculations, $this->mockBean);
        
        $expected = [
            'quote_id' => 'quote-123-456',
            'customer_id' => 'account-789',
            'currency' => 'USD',
            'items' => [],
            'discount' => 100.00,
            'shipping' => 25.50,
            'tax_rate' => 75.00
        ];
        
        $this->assertEquals($expected, $result);
    }
    
    public function testPrepareParametersWithLineItems()
    {
        // Mock line item
        $mockLineItem = $this->createMock(stdClass::class);
        $mockLineItem->product_id = 'product-123';
        $mockLineItem->product_qty = '2';
        $mockLineItem->product_unit_price = '199.99';
        $mockLineItem->product_discount = '10';
        $mockLineItem->product_discount_type = 'percentage';
        $mockLineItem->vat = '8.5';
        
        // Mock the line items relationship
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([$mockLineItem]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('prepareParameters');
        $method->setAccessible(true);
        
        $result = $method->invoke($this->cobolCalculations, $this->mockBean);
        
        $expectedItem = [
            'product_id' => 'product-123',
            'quantity' => 2.0,
            'unit_price' => 199.99,
            'discount' => 10.0,
            'discount_type' => 'percentage',
            'tax_rate' => 8.5
        ];
        
        $this->assertEquals([$expectedItem], $result['items']);
    }
    
    public function testCallCobolAPISuccess()
    {
        $apiUrl = 'http://test-api:3000/api/v1';
        $apiKey = 'test-key';
        $parameters = ['test' => 'data'];
        
        $expectedResponse = json_encode([
            'success' => true,
            'data' => [
                'subtotal' => 1000.00,
                'tax_amount' => 85.00,
                'total_amount' => 1085.00
            ]
        ]);
        
        // Mock curl functions
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['executeCurl'])
            ->getMock();
        
        $cobolCalculations->method('executeCurl')
            ->willReturn([
                'response' => $expectedResponse,
                'httpCode' => 200,
                'error' => null
            ]);
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('callCobolAPI');
        $method->setAccessible(true);
        
        $result = $method->invoke($cobolCalculations, $apiUrl, $apiKey, $parameters);
        
        $this->assertTrue($result['success']);
        $this->assertEquals(json_decode($expectedResponse, true), $result['data']);
    }
    
    public function testCallCobolAPIHttpError()
    {
        $apiUrl = 'http://test-api:3000/api/v1';
        $apiKey = 'test-key';
        $parameters = ['test' => 'data'];
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['executeCurl'])
            ->getMock();
        
        $cobolCalculations->method('executeCurl')
            ->willReturn([
                'response' => 'Internal Server Error',
                'httpCode' => 500,
                'error' => null
            ]);
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('callCobolAPI');
        $method->setAccessible(true);
        
        $result = $method->invoke($cobolCalculations, $apiUrl, $apiKey, $parameters);
        
        $this->assertFalse($result['success']);
        $this->assertEquals(500, $result['httpCode']);
    }
    
    public function testCallCobolAPICurlError()
    {
        $apiUrl = 'http://test-api:3000/api/v1';
        $apiKey = 'test-key';
        $parameters = ['test' => 'data'];
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['executeCurl'])
            ->getMock();
        
        $cobolCalculations->method('executeCurl')
            ->willReturn([
                'response' => '',
                'httpCode' => 0,
                'error' => 'Connection timeout'
            ]);
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('callCobolAPI');
        $method->setAccessible(true);
        
        $result = $method->invoke($cobolCalculations, $apiUrl, $apiKey, $parameters);
        
        $this->assertFalse($result['success']);
        $this->assertEquals('Connection timeout', $result['error']);
    }
    
    public function testApplyCalculationsBasic()
    {
        $calculations = [
            'subtotal' => 1000.00,
            'discount_amount' => 50.00,
            'tax_amount' => 85.00,
            'shipping_amount' => 25.00,
            'total_amount' => 1060.00,
            'notes' => 'Calculated using COBOL financial engine'
        ];
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('applyCalculations');
        $method->setAccessible(true);
        
        $method->invoke($this->cobolCalculations, $this->mockBean, $calculations);
        
        $this->assertEquals(1000.00, $this->mockBean->subtotal_amount);
        $this->assertEquals(50.00, $this->mockBean->discount_amount);
        $this->assertEquals(85.00, $this->mockBean->tax_amount);
        $this->assertEquals(25.00, $this->mockBean->shipping_amount);
        $this->assertEquals(1060.00, $this->mockBean->total_amount);
        $this->assertStringContains('COBOL Calculation Notes:', $this->mockBean->description);
    }
    
    public function testApplyCalculationsWithLineItems()
    {
        // Mock line item
        $mockLineItem = $this->createMock(stdClass::class);
        $mockLineItem->expects($this->once())->method('save');
        
        // Mock the line items relationship
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([$mockLineItem]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $calculations = [
            'subtotal' => 1000.00,
            'total_amount' => 1085.00,
            'line_items' => [
                [
                    'total_price' => 399.98,
                    'vat_amount' => 34.00
                ]
            ]
        ];
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('applyCalculations');
        $method->setAccessible(true);
        
        $method->invoke($this->cobolCalculations, $this->mockBean, $calculations);
        
        $this->assertEquals(399.98, $mockLineItem->product_total_price);
        $this->assertEquals(34.00, $mockLineItem->vat_amt);
    }
    
    public function testLogCalculation()
    {
        $parameters = ['test' => 'input'];
        $result = [
            'data' => [
                'subtotal' => 1000.00,
                'total_amount' => 1085.00
            ]
        ];
        $executionTime = 0.5;
        
        $this->mockDb->expects($this->once())
            ->method('query')
            ->with($this->stringContains('INSERT INTO cobol_calculation_log'));
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('logCalculation');
        $method->setAccessible(true);
        
        $method->invoke($this->cobolCalculations, $this->mockBean, $parameters, $result, $executionTime);
    }
    
    public function testEnsureLogTable()
    {
        $this->mockDb->expects($this->once())
            ->method('query')
            ->with($this->stringContains('CREATE TABLE IF NOT EXISTS cobol_calculation_log'));
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('ensureLogTable');
        $method->setAccessible(true);
        
        $method->invoke($this->cobolCalculations);
    }
    
    public function testCalculateWithCOBOLSuccessfulFlow()
    {
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['callCobolAPI', 'logCalculation'])
            ->getMock();
        
        $apiResponse = [
            'success' => true,
            'data' => [
                'subtotal' => 1000.00,
                'tax_amount' => 85.00,
                'total_amount' => 1085.00
            ]
        ];
        
        $cobolCalculations->method('callCobolAPI')
            ->willReturn($apiResponse);
        
        $cobolCalculations->expects($this->once())
            ->method('logCalculation');
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
        
        $this->assertEquals(1, $this->mockBean->cobol_calculated_c);
        $this->assertNotEmpty($this->mockBean->cobol_execution_time_c);
        $this->assertEquals(1000.00, $this->mockBean->subtotal_amount);
        $this->assertEquals(85.00, $this->mockBean->tax_amount);
        $this->assertEquals(1085.00, $this->mockBean->total_amount);
    }
    
    public function testCalculateWithCOBOLFailedAPI()
    {
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['callCobolAPI'])
            ->getMock();
        
        $apiResponse = [
            'success' => false,
            'error' => 'COBOL service unavailable'
        ];
        
        $cobolCalculations->method('callCobolAPI')
            ->willReturn($apiResponse);
        
        // Expect error to be logged
        $GLOBALS['log']->expects($this->once())
            ->method('error')
            ->with($this->stringContains('COBOL Quote Calculation failed'));
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
        
        // Bean should not be marked as calculated
        $this->assertEmpty($this->mockBean->cobol_calculated_c);
    }
    
    public function testParameterSanitization()
    {
        // Mock line item with potentially dangerous values
        $mockLineItem = $this->createMock(stdClass::class);
        $mockLineItem->product_id = 'product-123';
        $mockLineItem->product_qty = 'invalid'; // Non-numeric
        $mockLineItem->product_unit_price = null; // Null value
        $mockLineItem->product_discount = ''; // Empty string
        $mockLineItem->product_discount_type = 'percentage';
        $mockLineItem->vat = 'abc'; // Non-numeric
        
        // Mock the line items relationship
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([$mockLineItem]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $reflection = new ReflectionClass(CobolQuoteCalculations::class);
        $method = $reflection->getMethod('prepareParameters');
        $method->setAccessible(true);
        
        $result = $method->invoke($this->cobolCalculations, $this->mockBean);
        
        $item = $result['items'][0];
        
        // Values should be sanitized to floats
        $this->assertEquals(0.0, $item['quantity']); // invalid -> 0.0
        $this->assertEquals(0.0, $item['unit_price']); // null -> 0.0
        $this->assertEquals(0.0, $item['discount']); // empty -> 0.0
        $this->assertEquals(0.0, $item['tax_rate']); // abc -> 0.0
    }
    
    public function testConfigurationDefaults()
    {
        // Remove config values to test defaults
        unset($GLOBALS['sugar_config']['cobol_api_url']);
        unset($GLOBALS['sugar_config']['cobol_api_key']);
        
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['callCobolAPI'])
            ->getMock();
        
        // Capture the API call parameters
        $cobolCalculations->expects($this->once())
            ->method('callCobolAPI')
            ->with(
                'http://localhost:3000/api/v1', // Default URL
                '', // Default empty API key
                $this->anything()
            )
            ->willReturn(['success' => false, 'error' => 'test']);
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
    }
    
    public function testLongRunningCalculation()
    {
        $mockRelationship = $this->createMock(stdClass::class);
        $mockRelationship->method('getBeans')->willReturn([]);
        
        $this->mockBean->method('load_relationship')->willReturn(true);
        $this->mockBean->aos_products_quotes = $mockRelationship;
        
        $cobolCalculations = $this->getMockBuilder(CobolQuoteCalculations::class)
            ->onlyMethods(['callCobolAPI', 'logCalculation'])
            ->getMock();
        
        // Mock a slow API response
        $cobolCalculations->method('callCobolAPI')
            ->willReturnCallback(function() {
                usleep(100000); // 0.1 seconds
                return [
                    'success' => true,
                    'data' => ['total_amount' => 1000.00]
                ];
            });
        
        $cobolCalculations->expects($this->once())
            ->method('logCalculation')
            ->with(
                $this->anything(),
                $this->anything(),
                $this->anything(),
                $this->greaterThan(0.05) // Should be at least 0.05 seconds
            );
        
        $cobolCalculations->calculateWithCOBOL($this->mockBean, 'after_save', []);
        
        // Execution time should be recorded
        $this->assertGreaterThan(0, $this->mockBean->cobol_execution_time_c);
    }
    
    protected function tearDown(): void
    {
        // Clean up globals
        unset($GLOBALS['db']);
        unset($GLOBALS['current_user']);
        unset($GLOBALS['log']);
        unset($GLOBALS['sugar_config']);
        
        parent::tearDown();
    }
}

// Helper class to make testing curl easier
class TestableCobolQuoteCalculations extends CobolQuoteCalculations
{
    public function executeCurl($endpoint, $postFields, $headers, $timeout)
    {
        // This method can be mocked in tests
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $postFields);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
        curl_setopt($ch, CURLOPT_TIMEOUT, $timeout);
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $error = curl_error($ch);
        curl_close($ch);
        
        return [
            'response' => $response,
            'httpCode' => $httpCode,
            'error' => $error
        ];
    }
}
?>