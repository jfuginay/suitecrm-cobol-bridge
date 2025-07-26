       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDIT-CALCULATOR.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-DATA.
          05 WS-CUSTOMER-INCOME      PIC 9(8)V99.
          05 WS-CREDIT-AMOUNT       PIC 9(8)V99.
          05 WS-CREDIT-TERM         PIC 9(3).
          05 WS-EXISTING-DEBT       PIC 9(8)V99.
       
       01 WS-CALCULATIONS.
          05 WS-DEBT-TO-INCOME      PIC 9(3)V99.
          05 WS-MONTHLY-INCOME      PIC 9(8)V99.
          05 WS-MONTHLY-PAYMENT     PIC 9(8)V99.
          05 WS-TOTAL-DEBT          PIC 9(8)V99.
          05 WS-CREDIT-LIMIT        PIC 9(8)V99.
          05 WS-INTEREST-RATE       PIC 9(2)V99.
       
       01 WS-RESULTS.
          05 WS-APPROVED            PIC X(1).
          05 WS-APPROVAL-REASON     PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM CALCULATE-MONTHLY-INCOME.
           PERFORM CALCULATE-DEBT-RATIO.
           PERFORM DETERMINE-INTEREST-RATE.
           PERFORM CALCULATE-MONTHLY-PAYMENT.
           PERFORM EVALUATE-CREDIT-APPROVAL.
           PERFORM DETERMINE-CREDIT-LIMIT.
           GOBACK.
       
       CALCULATE-MONTHLY-INCOME.
           DIVIDE WS-CUSTOMER-INCOME BY 12 
               GIVING WS-MONTHLY-INCOME.
       
       CALCULATE-DEBT-RATIO.
           ADD WS-EXISTING-DEBT TO WS-CREDIT-AMOUNT
               GIVING WS-TOTAL-DEBT.
           DIVIDE WS-TOTAL-DEBT BY WS-CUSTOMER-INCOME
               GIVING WS-DEBT-TO-INCOME.
       
       DETERMINE-INTEREST-RATE.
           EVALUATE TRUE
               WHEN WS-DEBT-TO-INCOME < 20
                   MOVE 5.5 TO WS-INTEREST-RATE
               WHEN WS-DEBT-TO-INCOME < 30
                   MOVE 7.5 TO WS-INTEREST-RATE
               WHEN WS-DEBT-TO-INCOME < 40
                   MOVE 9.5 TO WS-INTEREST-RATE
               WHEN OTHER
                   MOVE 12.5 TO WS-INTEREST-RATE
           END-EVALUATE.
       
       CALCULATE-MONTHLY-PAYMENT.
           COMPUTE WS-MONTHLY-PAYMENT = 
               (WS-CREDIT-AMOUNT * (1 + WS-INTEREST-RATE / 100))
               / WS-CREDIT-TERM.
       
       EVALUATE-CREDIT-APPROVAL.
           EVALUATE TRUE
               WHEN WS-DEBT-TO-INCOME > 45
                   MOVE 'N' TO WS-APPROVED
                   MOVE 'Debt-to-income ratio too high' 
                       TO WS-APPROVAL-REASON
               WHEN WS-MONTHLY-PAYMENT > (WS-MONTHLY-INCOME * 0.3)
                   MOVE 'N' TO WS-APPROVED
                   MOVE 'Monthly payment exceeds 30% of income' 
                       TO WS-APPROVAL-REASON
               WHEN WS-CUSTOMER-INCOME < 25000
                   MOVE 'N' TO WS-APPROVED
                   MOVE 'Income below minimum requirement' 
                       TO WS-APPROVAL-REASON
               WHEN OTHER
                   MOVE 'Y' TO WS-APPROVED
                   MOVE 'Credit approved' TO WS-APPROVAL-REASON
           END-EVALUATE.
       
       DETERMINE-CREDIT-LIMIT.
           IF WS-APPROVED = 'Y'
               COMPUTE WS-CREDIT-LIMIT = 
                   WS-CUSTOMER-INCOME * 0.2
           ELSE
               MOVE ZERO TO WS-CREDIT-LIMIT
           END-IF.