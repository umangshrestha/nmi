from tokens import *
from ast import *


___all__ = ["Parser"]

class Parser:
    def __init__(self, lex):
        self.lex = lex
        self.program = []
        self.curr_token = None
        self.next_token = None
        self.update()
        self.update()

    def update(self):
        self.curr_token = self.next_token
        self.next_token = next(self.lex)
        while (self.next_token == Token.WHITESPACE) or (self.next_token == Token.COMMENT): 
            self.next_token = next(self.lex)
        
    def is_next(self, t):
        if self.next_token != t:
            raise AssertionError(f"expected {t} but got {self.next_token.name}")
        self.update()    

    def parse_program(self) -> Statement: 
        ''' This is the generator that need to be called to get the statement.'''
        while self.curr_token.name != EOF:
            if statement := self.parse_statement():
                yield statement
      
    def parse_statement(self) -> Statement:
        if self.curr_token == ILLEGAL:   
            raise SyntaxError(f"invalid input: {self.curr_token.value}")
        elif expression := self.parse_let_statement(): 
            return expression 
        elif expression := self.print_statememt(): 
            return expression 
        elif expression :=  self.parse_assign_statement():
            return expression
        else:  
            expression = self.parse_expression(Priority.LOWEST)
            if self.next_token == Token.SEMICOLON:
                self.update()
            return expression 
    
    def print_statememt(self) -> Statement:
        if self.curr_token == Token.PRINT:
            state = self.curr_token.value
            self.is_next(Token.LPAREN)
            self.update()
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.RPAREN)
            self.is_next(Token.SEMICOLON) 
            self.update()
            return PrintStatement(state, value)

    def parse_let_statement(self) -> Statement: 
        if self.curr_token == Token.LET:
            self.is_next(Token.ID) 
            variable = self.curr_token.value
            self.is_next(Token.ASSIGN) # =
            self.update()
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.SEMICOLON) 
            self.update() 
            return LetStatement(variable, value)   
        
    def parse_assign_statement(self) -> Statement: 
        if self.curr_token == Token.ID:
            variable = self.curr_token.value
            self.is_next(Token.ASSIGN) # =
            self.update()
            value = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.SEMICOLON) 
            self.update() 
            return AssignStatement(variable, value)   

    def parse_expression(self, precedence: Priority) -> Expression: 
        expression = self.parse_datatypes()
        if expression == None:
            expression = self.parse_unary()
        if expression == None:
            raise SyntaxError(f"operand '{self.curr_token.value}' not defined")
        
        infix_list = [
            # athemetic oeprator
            Token.PLUS,    Token.MINUS,   
            Token.DIVIDE,  Token.TIMES,   
            Token.MODULUS, Token.POWER,
            # comparision operator
            Token.EQUAL,   Token.LARGE,   
            Token.LARGEEQ, Token.SMALL,   
            Token.SMALLEQ, Token.NOTEQ ,
            # logical operator 
            Token.AND,      Token.OR
        ]
        while self.next_token != Token.SEMICOLON and \
            precedence <= get_precedence(self.next_token):
            # infix function needs to call infix list
            if self.next_token.name in infix_list:
                self.update()
                expression = self.parse_infix_expression(expression)
            else:
                break
        return expression

    def parse_infix_expression(self, left: Expression) -> Expression:
            operator = self.curr_token.value
            precedence = get_precedence(self.curr_token) 
            self.update()
            right = self.parse_expression(precedence)
            return InfixExpression(left, operator , right)

    def parse_datatypes(self) -> Expression:
        datatypes =  {
            Token.INT.name   : IntegerLiteral,
            Token.FLOAT.name : FloatLiteral,
            Token.TRUE.name  : BooleanLiteral,
            Token.FALSE.name : BooleanLiteral,
            Token.STRING.name: StringLiteral,
        }
        if dt := datatypes.get(self.curr_token.name, None):  
            return  dt(self.curr_token.name, self.curr_token.value)
        elif self.curr_token == Token.ID:
            return Identifier( self.curr_token.value)
   
    def parse_unary(self):
        if self.curr_token.name in  [Token.NOT, Token.MINUS]:
            operator = self.curr_token.value
            precedence = get_precedence(self.curr_token) 
            self.update()
            right = self.parse_expression(Priority.HIGHER)
            return PrefixExpression(operator, right)
        elif self.curr_token.name ==  Token.LPAREN: 
            self.update()
            expression = self.parse_expression(Priority.LOWEST)
            self.is_next(Token.RPAREN)
            return expression 
      