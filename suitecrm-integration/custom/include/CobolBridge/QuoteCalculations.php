<?php
/**
 * COBOL Quote Calculations
 * 
 * Logic hook for AOS_Quotes to perform calculations using COBOL
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class CobolQuoteCalculations
{
    /**
     * Calculate quote using COBOL financial calculation programs
     */
    public function calculateWithCOBOL($bean, $event, $arguments)
    {
        global $sugar_config;
        
        // Skip if already calculated or if in import
        if (!empty($bean->cobol_calculated_c) || !empty($bean->in_import)) {
            return;
        }
        
        // Prepare calculation parameters
        $parameters = $this->prepareParameters($bean);
        
        // Get API configuration
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        // Call COBOL API for financial calculations
        $startTime = microtime(true);
        $result = $this->callCobolAPI($apiUrl, $apiKey, $parameters);
        $executionTime = microtime(true) - $startTime;
        
        if ($result['success']) {
            // Apply calculations to quote
            $this->applyCalculations($bean, $result['data']);
            
            // Set custom fields
            $bean->cobol_calculated_c = 1;
            $bean->cobol_execution_time_c = $executionTime;
            
            // Log successful calculation
            $this->logCalculation($bean, $parameters, $result, $executionTime);
        } else {
            // Log error but don't prevent save
            $GLOBALS['log']->error('COBOL Quote Calculation failed: ' . json_encode($result));
        }
    }
    
    /**
     * Prepare parameters for COBOL calculation
     */
    protected function prepareParameters($bean)
    {
        $parameters = array(
            'quote_id' => $bean->id,
            'customer_id' => $bean->billing_account_id,
            'currency' => $bean->currency_id,
            'items' => array(),
        );
        
        // Get line items
        if ($bean->load_relationship('aos_products_quotes')) {
            $lineItems = $bean->aos_products_quotes->getBeans();
            
            foreach ($lineItems as $lineItem) {
                $parameters['items'][] = array(
                    'product_id' => $lineItem->product_id,
                    'quantity' => floatval($lineItem->product_qty),
                    'unit_price' => floatval($lineItem->product_unit_price),
                    'discount' => floatval($lineItem->product_discount),
                    'discount_type' => $lineItem->product_discount_type,
                    'tax_rate' => floatval($lineItem->vat),
                );
            }
        }
        
        // Add quote-level parameters
        $parameters['discount'] = floatval($bean->discount_amount);
        $parameters['shipping'] = floatval($bean->shipping_amount);
        $parameters['tax_rate'] = floatval($bean->tax_amount);
        
        return $parameters;
    }
    
    /**
     * Call COBOL API for calculations
     */
    protected function callCobolAPI($apiUrl, $apiKey, $parameters)
    {
        $endpoint = $apiUrl . '/cobol/financial/calculate-quote';
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($parameters));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Authorization: Bearer ' . $apiKey
        ));
        curl_setopt($ch, CURLOPT_TIMEOUT, 10); // 10 second timeout
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $error = curl_error($ch);
        curl_close($ch);
        
        if ($error) {
            return array(
                'success' => false,
                'error' => $error,
            );
        }
        
        if ($httpCode !== 200) {
            return array(
                'success' => false,
                'httpCode' => $httpCode,
                'response' => $response,
            );
        }
        
        return array(
            'success' => true,
            'data' => json_decode($response, true),
        );
    }
    
    /**
     * Apply COBOL calculations to quote
     */
    protected function applyCalculations($bean, $calculations)
    {
        // Apply calculated totals
        if (isset($calculations['subtotal'])) {
            $bean->subtotal_amount = $calculations['subtotal'];
        }
        
        if (isset($calculations['discount_amount'])) {
            $bean->discount_amount = $calculations['discount_amount'];
        }
        
        if (isset($calculations['tax_amount'])) {
            $bean->tax_amount = $calculations['tax_amount'];
        }
        
        if (isset($calculations['shipping_amount'])) {
            $bean->shipping_amount = $calculations['shipping_amount'];
        }
        
        if (isset($calculations['total_amount'])) {
            $bean->total_amount = $calculations['total_amount'];
        }
        
        // Apply line item calculations if provided
        if (isset($calculations['line_items']) && $bean->load_relationship('aos_products_quotes')) {
            $lineItems = $bean->aos_products_quotes->getBeans();
            
            foreach ($lineItems as $index => $lineItem) {
                if (isset($calculations['line_items'][$index])) {
                    $itemCalc = $calculations['line_items'][$index];
                    
                    if (isset($itemCalc['total_price'])) {
                        $lineItem->product_total_price = $itemCalc['total_price'];
                    }
                    
                    if (isset($itemCalc['vat_amount'])) {
                        $lineItem->vat_amt = $itemCalc['vat_amount'];
                    }
                    
                    $lineItem->save();
                }
            }
        }
        
        // Add calculation notes if provided
        if (isset($calculations['notes'])) {
            $bean->description = trim($bean->description . "\n\nCOBOL Calculation Notes:\n" . $calculations['notes']);
        }
    }
    
    /**
     * Log calculation for audit trail
     */
    protected function logCalculation($bean, $parameters, $result, $executionTime)
    {
        global $db, $current_user;
        
        $logData = array(
            'id' => create_guid(),
            'quote_id' => $bean->id,
            'user_id' => $current_user->id,
            'calculation_date' => date('Y-m-d H:i:s'),
            'input_parameters' => json_encode($parameters),
            'output_result' => json_encode($result['data']),
            'execution_time' => $executionTime,
            'status' => 'success',
        );
        
        // Create log table if it doesn't exist
        $this->ensureLogTable();
        
        // Insert log record
        $query = "INSERT INTO cobol_calculation_log 
                  (id, quote_id, user_id, calculation_date, input_parameters, output_result, execution_time, status) 
                  VALUES 
                  ('{$logData['id']}', '{$logData['quote_id']}', '{$logData['user_id']}', 
                   '{$logData['calculation_date']}', '{$db->quote($logData['input_parameters'])}', 
                   '{$db->quote($logData['output_result'])}', {$logData['execution_time']}, '{$logData['status']}')";
        
        $db->query($query);
    }
    
    /**
     * Ensure log table exists
     */
    protected function ensureLogTable()
    {
        global $db;
        
        $query = "CREATE TABLE IF NOT EXISTS cobol_calculation_log (
            id CHAR(36) NOT NULL PRIMARY KEY,
            quote_id CHAR(36) NOT NULL,
            user_id CHAR(36) NOT NULL,
            calculation_date DATETIME NOT NULL,
            input_parameters TEXT,
            output_result TEXT,
            execution_time FLOAT,
            status VARCHAR(50),
            INDEX idx_quote_id (quote_id),
            INDEX idx_calculation_date (calculation_date)
        )";
        
        $db->query($query);
    }
}
?>