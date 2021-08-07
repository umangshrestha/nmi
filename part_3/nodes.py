
class Node:  ...
class Statement(Node): ...
class Expression(Node): ...

from tokens import *

class Literal(Expression):
    def __init__(self, typ: str, value: str):
        self.type = typ
        self.value = value

    def __repr__(self) -> str:
        return str(self.value)
    
    def get_type(self) -> str:
        # returns data type
        if self.type == Token.FALSE or self.type == Token.TRUE:
            return "bool" 
        return self.type.lower()

    def type_casting(self):
        # conversion will be done here for the value to 
        # actual value
        if self.type == Token.INT:
            return int(self.value)
        elif self.type == Token.FLOAT:
            return float(self.value)
        elif self.type ==  Token.STRING:
            return str(self.value)
        elif self.type == Token.TRUE :
            return True 
        elif self.type == Token.FALSE:
            return False
        else:
            return None

    def eval(self):
        try:
            out = self.type_casting()
        except ValueError:
            raise ValueError(f"Error converting {self.value} to {self.type}")
        return out



class InfixExpression(Expression):
    def __init__(self, left: Expression, operator: Statement, right: Expression):
        self.left = left
        self.operator = operator
        self.right = right

    def __repr__(self) -> str:
        return f"({self.left} {self. operator} {self.right})"

    def eval(self):
        operator =  {
            "%" : lambda a,b: a%b,
            "^" : lambda a,b: a**b,
            "+" : lambda a,b: a+b,
            "-" : lambda a,b: a-b,
            "*" : lambda a,b: a*b,
            "/" : lambda a,b: a//b,
            "&" : lambda a,b: a and b,
            "|" : lambda a,b: a or b,
        }
        try:
            return operator[self.operator](self.left.eval(), self.right.eval()) 
        except TypeError:
            raise TypeError(f"can't perform {self.operator} between {self.left_type()} and  {self.left_type()}")

class PrefixExpression(Expression):
    def __init__(self, operator: Statement, right: Expression):
        self.operator = operator
        self.right = right

    def get_type(self) -> str:
        # returns data type
        return self.type.right

    def __repr__(self) -> str:
        return f"({self. operator}{self.right})"

    def eval(self):
        operator =  {
            "!" : lambda a: not a,
            "-" : lambda a: -a,
        }
        try:
            return operator[self.operator](self.right.eval())  
        except TypeError:
            raise TypeError(f"can't perform {self.operator} between {self.left_type()} and  {self.left_type()}")
 