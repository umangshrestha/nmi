from interface import Expression, Statement
from tokens import TokenInfo, Token
from typing import Callable, Dict


class Identifier(Expression):
    def __init__(self, tok: TokenInfo):
        self.token  = tok 
        self.value  = tok.literal       

    def __repr__(self) -> str:
        return self.value


class IntegerLiteral(Expression):
    def __init__(self, tok: TokenInfo):
        self.token  = tok 
        self.value  = int(self.token.literal) 
    
    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return self.token.literal


class BooleanLiteral(Expression):
    def __init__(self, tok: TokenInfo):
        self.token  = tok 
        self.value  = True if tok.type == Token.TRUE else False 
    
    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return self.token.literal

    
class FloatLiteral(Expression):
    def __init__(self, tok: TokenInfo):
        self.token  = tok  
        self.value  = float(tok.literal) 

    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return self.token.literal
    

class StringLiteral(Expression):
    def __init__(self, tok: TokenInfo):
        self.token  = tok  
        self.value  = str(tok.literal) 

    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return self.token.literal
    

class InfixExpression(Expression):
    def __init__(self, tok: TokenInfo,  left: Expression, right: Expression):
        self.token  = tok  
        self.left = left 
        self.right = right 
        self.operator = tok.literal 

    def __repr__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"


class PrefixExpression(Expression):
    def __init__(self, tok: TokenInfo,  right: Expression):
        self.token  = tok  
        self.right = right 
        self.operator = tok.literal 

    def __repr__(self) -> str:
        return f"({self.operator} {self.right}"
   

class IfExpression(Expression):
    def __init__(self, tok: TokenInfo,  condition: Expression, branch1: Statement, branch2: Statement):
        self.token     =    tok  
        self.condition = condition 
        self.branch1   = branch1 
        self.branch2   = branch2 

    def __repr__(self)->str:
        data = f"if {condition.__repr__()} {branch1.__repr__}"
        if branch2 != None:
            data += f" else {branch2.__repr__()}"
        return data
    

