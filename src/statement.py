from tokens import TokenInfo
from interface import Statement, Expression
from expression import Identifier


class LetStatement(Statement):
    def __init__(self, tok: TokenInfo, name: Identifier, value: Expression):
        self.token  = tok 
        self.name   = name   
        self.value  = value 

    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return f"{self.get_literal()} {self.name} = {self.value};"


class ExpressionStatement(Statement):
    def __init__(self, tok: TokenInfo, expression: Expression):
        self.token  = tok  
        self.expression  = expression 
    
    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return self.expression.__repr__()
    

class ReturnStatement(Statement):
    def __init__(tok: TokenInfo, value: Expression):
        self.token  = tok  
        self.value  = value 

    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return f"{self.get_token_literal()}  {self.value.__repr__() if self.value else ''};"


class BlockStatement(Statement):
    def __init__(tok: TokenInfo, value: Expression):
        self.token  = tok  
        self.statements  = [] 

    def get_literal(self) -> TokenInfo.literal:
        return self.token.literal 

    def __repr__(self) -> str:
        return f"{self.get_token_literal()}  {statement.__repr__() for statement in self.statements};"









            
        
    

