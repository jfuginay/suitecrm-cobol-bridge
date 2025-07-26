import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';
import { message } from 'antd';
import RuleEditor from '../RuleEditor';

// Mock antd message
jest.mock('antd', () => {
  const actualAntd = jest.requireActual('antd');
  return {
    ...actualAntd,
    message: {
      success: jest.fn(),
      error: jest.fn(),
      warning: jest.fn()
    }
  };
});

// Mock react-dnd
jest.mock('react-dnd', () => ({
  DndProvider: ({ children }) => children
}));

jest.mock('react-dnd-html5-backend', () => ({
  HTML5Backend: {}
}));

describe('RuleEditor', () => {
  const mockOnRuleUpdate = jest.fn();
  const mockOnRuleAdd = jest.fn();
  const mockOnRuleDelete = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('when no node is selected', () => {
    it('should render empty state with add rule button', () => {
      render(
        <RuleEditor
          selectedNode={null}
          onRuleUpdate={mockOnRuleUpdate}
          onRuleAdd={mockOnRuleAdd}
          onRuleDelete={mockOnRuleDelete}
        />
      );

      expect(screen.getByText('Select a node from the decision tree to edit')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /add new rule/i })).toBeInTheDocument();
    });

    it('should open add rule modal when add button clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));

      expect(screen.getByText('Add New Rule')).toBeInTheDocument();
      expect(screen.getByLabelText(/rule type/i)).toBeInTheDocument();
    });

    it('should close add rule modal when cancel clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));
      await user.click(screen.getByRole('button', { name: /cancel/i }));

      expect(screen.queryByText('Add New Rule')).not.toBeInTheDocument();
    });
  });

  describe('when decision node is selected', () => {
    const decisionNode = {
      id: 'decision-1',
      data: {
        type: 'decision',
        rule: {
          type: 'decision',
          condition: {
            type: 'comparison',
            field: 'CUSTOMER-TYPE',
            operator: '=',
            value: 'PREMIUM'
          },
          subject: 'Customer Classification'
        }
      }
    };

    it('should render decision rule editor', () => {
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
          onRuleDelete={mockOnRuleDelete}
        />
      );

      expect(screen.getByText('DECISION')).toBeInTheDocument();
      expect(screen.getByLabelText(/condition/i)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /edit/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /delete/i })).toBeInTheDocument();
    });

    it('should format condition for display', () => {
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      const conditionInput = screen.getByLabelText(/condition/i);
      expect(conditionInput).toHaveValue('CUSTOMER-TYPE = PREMIUM');
    });

    it('should enable editing when edit button clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));

      expect(screen.getByRole('button', { name: /save/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
      
      // Form fields should be enabled
      const conditionInput = screen.getByLabelText(/condition/i);
      expect(conditionInput).not.toBeDisabled();
    });

    it('should save changes when save button clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));
      
      const conditionInput = screen.getByLabelText(/condition/i);
      await user.clear(conditionInput);
      await user.type(conditionInput, 'AMOUNT > 1000');

      await user.click(screen.getByRole('button', { name: /save/i }));

      await waitFor(() => {
        expect(mockOnRuleUpdate).toHaveBeenCalledWith(
          'decision-1',
          expect.objectContaining({
            condition: 'AMOUNT > 1000'
          })
        );
      });

      expect(message.success).toHaveBeenCalledWith('Rule updated successfully');
    });

    it('should cancel editing when cancel button clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));
      await user.click(screen.getByRole('button', { name: /cancel/i }));

      expect(screen.getByRole('button', { name: /edit/i })).toBeInTheDocument();
      expect(screen.queryByRole('button', { name: /save/i })).not.toBeInTheDocument();
    });

    it('should show delete confirmation when delete button clicked', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleDelete={mockOnRuleDelete}
        />
      );

      await user.click(screen.getByRole('button', { name: /delete/i }));

      expect(screen.getByText('Delete Rule')).toBeInTheDocument();
      expect(screen.getByText('Are you sure you want to delete this rule?')).toBeInTheDocument();
    });

    it('should delete rule when confirmed', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleDelete={mockOnRuleDelete}
        />
      );

      await user.click(screen.getByRole('button', { name: /delete/i }));
      await user.click(screen.getByRole('button', { name: /delete/i })); // Confirm deletion

      expect(mockOnRuleDelete).toHaveBeenCalledWith('decision-1');
      expect(message.success).toHaveBeenCalledWith('Rule deleted successfully');
    });
  });

  describe('when action node is selected', () => {
    const actionNode = {
      id: 'action-1',
      data: {
        type: 'action',
        action: {
          type: 'assignment',
          source: 'HIGH-LIMIT',
          target: 'CREDIT-LIMIT'
        }
      }
    };

    it('should render action editor', () => {
      render(
        <RuleEditor
          selectedNode={actionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      expect(screen.getByText('Action Details')).toBeInTheDocument();
      expect(screen.getByDisplayValue('assignment')).toBeInTheDocument();
    });

    it('should render assignment action fields', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={actionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));

      expect(screen.getByLabelText(/source/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/target/i)).toBeInTheDocument();
      expect(screen.getByDisplayValue('HIGH-LIMIT')).toBeInTheDocument();
      expect(screen.getByDisplayValue('CREDIT-LIMIT')).toBeInTheDocument();
    });
  });

  describe('when evaluate node is selected', () => {
    const evaluateNode = {
      id: 'evaluate-1',
      data: {
        type: 'evaluate',
        rule: {
          type: 'evaluate',
          subject: 'CREDIT-SCORE',
          cases: [
            { condition: 'WHEN > 700', actions: ['SET PREMIUM'] },
            { condition: 'WHEN 600-700', actions: ['SET STANDARD'] }
          ]
        }
      }
    };

    it('should render evaluate rule editor', () => {
      render(
        <RuleEditor
          selectedNode={evaluateNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      expect(screen.getByDisplayValue('CREDIT-SCORE')).toBeInTheDocument();
      expect(screen.getByText('Cases')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /add case/i })).toBeInTheDocument();
    });
  });

  describe('condition formatting', () => {
    it('should format compound conditions', () => {
      const compoundNode = {
        id: 'compound-1',
        data: {
          type: 'decision',
          rule: {
            condition: {
              type: 'compound',
              operator: 'AND',
              conditions: [
                { type: 'comparison', field: 'AGE', operator: '>', value: '18' },
                { type: 'comparison', field: 'INCOME', operator: '>=', value: '50000' }
              ]
            }
          }
        }
      };

      render(
        <RuleEditor
          selectedNode={compoundNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      const conditionInput = screen.getByLabelText(/condition/i);
      expect(conditionInput).toHaveValue('AGE > 18 AND INCOME >= 50000');
    });

    it('should format function conditions', () => {
      const functionNode = {
        id: 'function-1',
        data: {
          type: 'decision',
          rule: {
            condition: {
              type: 'function',
              function: 'LENGTH',
              argument: 'CUSTOMER-NAME'
            }
          }
        }
      };

      render(
        <RuleEditor
          selectedNode={functionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      const conditionInput = screen.getByLabelText(/condition/i);
      expect(conditionInput).toHaveValue('LENGTH(CUSTOMER-NAME)');
    });

    it('should format boolean conditions', () => {
      const booleanNode = {
        id: 'boolean-1',
        data: {
          type: 'decision',
          rule: {
            condition: {
              type: 'boolean',
              field: 'IS-PREMIUM-CUSTOMER'
            }
          }
        }
      };

      render(
        <RuleEditor
          selectedNode={booleanNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      const conditionInput = screen.getByLabelText(/condition/i);
      expect(conditionInput).toHaveValue('IS-PREMIUM-CUSTOMER');
    });
  });

  describe('add new rule modal', () => {
    it('should render decision rule form when decision type selected', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));
      
      const ruleTypeSelect = screen.getByLabelText(/rule type/i);
      await user.click(ruleTypeSelect);
      await user.click(screen.getByText('Decision (IF)'));

      expect(screen.getByLabelText(/field/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/operator/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/value/i)).toBeInTheDocument();
    });

    it('should render evaluate rule form when evaluate type selected', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));
      
      const ruleTypeSelect = screen.getByLabelText(/rule type/i);
      await user.click(ruleTypeSelect);
      await user.click(screen.getByText('Evaluate (SWITCH)'));

      expect(screen.getByLabelText(/subject/i)).toBeInTheDocument();
    });

    it('should add new rule when form submitted', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));
      
      const ruleTypeSelect = screen.getByLabelText(/rule type/i);
      await user.click(ruleTypeSelect);
      await user.click(screen.getByText('Decision (IF)'));

      await user.type(screen.getByLabelText(/field/i), 'BALANCE');
      
      const operatorSelect = screen.getByLabelText(/operator/i);
      await user.click(operatorSelect);
      await user.click(screen.getByText('>'));

      await user.type(screen.getByLabelText(/value/i), '1000');

      await user.click(screen.getByRole('button', { name: 'Add Rule' }));

      await waitFor(() => {
        expect(mockOnRuleAdd).toHaveBeenCalledWith(
          expect.objectContaining({
            type: 'decision',
            field: 'BALANCE',
            operator: '>',
            value: '1000'
          })
        );
      });

      expect(message.success).toHaveBeenCalledWith('New rule added successfully');
    });

    it('should validate required fields', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={null}
          onRuleAdd={mockOnRuleAdd}
        />
      );

      await user.click(screen.getByRole('button', { name: /add new rule/i }));
      await user.click(screen.getByRole('button', { name: 'Add Rule' }));

      // Should show validation error for required field
      await waitFor(() => {
        expect(screen.getByText(/please select rule type/i)).toBeInTheDocument();
      });

      expect(mockOnRuleAdd).not.toHaveBeenCalled();
    });
  });

  describe('action builder', () => {
    const actionNode = {
      id: 'action-1',
      data: {
        type: 'action',
        action: {
          type: 'computation',
          target: 'TOTAL-AMOUNT',
          expression: 'PRINCIPAL + INTEREST'
        }
      }
    };

    it('should render computation action fields', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={actionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));

      expect(screen.getByDisplayValue('TOTAL-AMOUNT')).toBeInTheDocument();
      expect(screen.getByDisplayValue('PRINCIPAL + INTEREST')).toBeInTheDocument();
    });

    it('should change fields when action type changes', async () => {
      const user = userEvent.setup();
      render(
        <RuleEditor
          selectedNode={actionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));
      
      const actionTypeSelect = screen.getByDisplayValue('Computation (COMPUTE)');
      await user.click(actionTypeSelect);
      await user.click(screen.getByText('Display Message'));

      expect(screen.getByLabelText(/message/i)).toBeInTheDocument();
      expect(screen.queryByLabelText(/expression/i)).not.toBeInTheDocument();
    });
  });

  describe('error handling', () => {
    it('should handle missing node data gracefully', () => {
      const emptyNode = {
        id: 'empty-1',
        data: {}
      };

      render(
        <RuleEditor
          selectedNode={emptyNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      // Should render without crashing
      expect(screen.getByText('Rule Editor')).toBeInTheDocument();
    });

    it('should handle form validation errors', async () => {
      const user = userEvent.setup();
      const decisionNode = {
        id: 'decision-1',
        data: {
          type: 'decision',
          rule: { type: 'decision', condition: {} }
        }
      };

      // Mock form validation to fail
      const mockValidateFields = jest.fn().mockRejectedValue(new Error('Validation failed'));
      
      render(
        <RuleEditor
          selectedNode={decisionNode}
          onRuleUpdate={mockOnRuleUpdate}
        />
      );

      await user.click(screen.getByRole('button', { name: /edit/i }));
      
      // Mock the form validation
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);

      // Should handle validation error gracefully
      await waitFor(() => {
        expect(mockOnRuleUpdate).not.toHaveBeenCalled();
      });
    });
  });
});