from typing import List, Callable, Dict
from tokens import Token, TokenInfo
from lexer import Lexer
from interface import Statement, Program,  Expression 
from statement import *
from expression import *
from enum import Enum
from precedence import *


# infix function needs to call infix list
infix_list: List[Token] = [
    # athemetic oeprator
    Token.PLUS,    
    Token.MINUS,   
    Token.DIVIDE,  
    Token.TIMES,   
    Token.MODULUS, 
    # comparision operator
    Token.EQUAL,   
    Token.LARGE,   
    Token.LARGEEQ, 
    Token.SMALL,   
    Token.SMALLEQ,   
]

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token: TokenInfo = self.lexer.next_token()
        self.next_token: TokenInfo = self.lexer.next_token()
        self.errors: List[string] = []
  
    def read_next_token(self):
        self.cur_token  = self.next_token
        self.next_token = self.lexer.next_token()

    def remove_newline(self):
        self.read_token()
        while self.cur_token != "\n":
            self.read_token() 

    def parse_program(self) -> Program:
        program = Program()
        while self.cur_token.type != Token.EOF:
            statement = self.parse_statement()
            if statement != None:
                program.statements.append(statement)
            self.read_next_token()
        return program
    
    def parse_statement(self)->Statement:
        if self.cur_token.type == Token.LET:
            return self.parse_let_statement()
        elif self.cur_token.type == Token.RETURN: 
            return self.parse_return_statement,
        else:
            return self.parse_expression_statement()
      
    def parse_expression_statement(self) -> Statement:
        token = self.cur_token
        expression = self.parse_expression(Priority.LOWEST)
        if self.next_token == Token.SEMICOLON:
            p.read_next_token()
        return ExpressionStatement(token, expression) 
       
    def parse_expression(self, precedence: Priority) -> Expression: 
        prefix_datatypes: Dict[Token, Callable] = {
            Token.ID    : Identifier,
            Token.INT   : IntegerLiteral,
            Token.FLOAT : FloatLiteral,
            Token.TRUE  : BooleanLiteral,
            Token.FALSE : BooleanLiteral,
            Token.STRING: StringLiteral,
        }
        prefix = prefix_datatypes.get(self.cur_token.type, None)
        if prefix != None:    
            try:
                expression = prefix(self.cur_token)
            except ValueError:
                error = f"{t.lineno}:{t.linepos} error parsing \"{self.cur_token.value}\" as {self.cur_token.type}"
                self.errors.append(error)
                return None
        else:
            prefix_function: Dict[token, Callable] = {
                # parse prefix
                Token.NOT  : self.parse_prefix_expression,
                Token.MINUS : self.parse_prefix_expression,
                # parse expression
                Token.LPAREN: self.parse_grouped_expresssion,
                Token.IF    : self.parse_if_expresssion,  
                Token.FUNCTION : self.parse_if_expresssion,  
            } 
            prefix = prefix_function.get(self.cur_token, None)
            if prefix == None:
                return None
        while self.next_token.type != Token.SEMICOLON and \
            Priority.LOWEST.value < get_precedence(self.next_token.type).value:
            if self.next_token.type in infix_list:
                self.read_next_token()
                self.parse_infix_expression(expression)
            elif self.next_token.type == Token.LPAREN:
                self.read_next_token()
                self.parse_call_expression()
            else:
                return expression
            self.read_next_token()
        return expression
   
    def parse_infix_expression(self, left: Expression) -> Expression:
        token = self.cur_token
        precedence = get_precedence(self.cur_token.type) 
        self.read_next_token()
        right = self.parse_expression(precedence)
        return InfixExpression(token, left, right)

    def parse_prefix_expression(self) -> Expression:
        token = self.cur_token
        precedence = self.get_precedence(self.cur_token) 
        self.read_next_token()
        right = self.parse_expression(precedence)
        prntt(token)
        return InfixExpression(token, right)

    def parse_grouped_expresssion(self) -> Expression:
        self.read_next_token()
        expression = self.parse_expression(Priority.LOWEST)
        return expression if self.next_token.type != Token.RPAREN else None
        
    def parse_let_statement(self) -> Statement:
        print("let statement")
        tok = self.cur_token
        if not self.expect_next(Token.ID):
            return None
        name = Identifier(self.cur_token)
        if not self.expect_next(Token.ASSIGN):
            return None
        self.read_next_token()
        value = self.parse_expression(Priority.LOWEST)
        if self.expect_next(Token.SEMICOLON):
            self.read_next_token()
        return LetStatement(tok, name, value)

    def parse_if_expresssion(self) -> Expression:
        print("if statement")
        tok = self.cur_token
        if not self.expect_next(Token.LPAREN):
            return None
        self.next_token()
        condition1 = self.parse_expression()
        
        if not self.expect_next(Token.RPAREN):
            return None
        if not self.expect_next(Token.LBRACE):
            return None
        condition2: self.parse_block_statement()

        if self.next_token.type == Token.ELSE:
            self.read_next_token()
            if not self.expect_next(Token.LBRACE):
                return None
        value = self.expresssion()
        if self.expect_next(Token.SEMICOLON):
            self.read_next_token()
        return IFStatement(tok, name, value)

    def parse_return_statement(self) -> Statement:
        print("return statement")
        tok = self.cur_token
        self.read_next_token()
        value = self.parse_expression(Priority.LOWEST)
        if self.expect_next(Token.SEMICOLON):
            self.read_next_token()
        return ReturnStatement(token, value)

    def expect_next(self, t: TokenInfo.type) -> bool:
        if self.next_token.type == t:
            self.read_next_token()
            return True
        else:
            l = self.cur_token
            self.errors.append(f"{l.line}:{l.pos} expected: {t} received: {self.next_token.type}")
            return False
