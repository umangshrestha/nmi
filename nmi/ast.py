class Node:  pass
class Statement(Node): pass
class Expression(Node): pass

class Literal(Expression):
    def __init__(self, typ: str, value: str):
        self.type = typ
        self.value = value

    def __repr__(self) -> str:
        return str(self.value)
    
    def get_type(self) -> str:
        return self.type

    def type_casting(self):
        pass

    def eval(self):
        try:
            out = self.type_casting()
        except ValueError:
            raise(f"Error converting {self.value} to {self.type}")
        return out


class IntegerLiteral(Literal):    
    def type_casting(self) -> int:
       return int(self.value)

class FloatLiteral(Literal):    
    def type_casting(self) -> float:
       return float(self.value)

class StringLiteral(Literal):    
    def type_casting(self) -> str:
       return str(self.value)

class BooleanLiteral(Literal):    
    def type_casting(self) -> bool:
       return True if self.type == "TRUE" else False


storage = {}


class LetStatement(Statement):
    def __init__(self,name: str, expr: Expression):   
        self.name = name
        self.expr = expr

    def __repr__(self) -> str:
        return f"(set {self.name} {self.expr})"

    def eval(self):
        storage[self.name] = self.expr.eval()

class AssignStatement(Statement):
    def __init__(self,name: str, expr: Expression):   
        self.name = name
        self.expr = expr

    def __repr__(self) -> str:
        return f"(set {self.name} {self.expr})"

    def eval(self):
        if self.name not in storage:
            raise NameError(f'"{self.name}" not defined')
        storage[self.name] = self.expr.eval()

class Identifier(Expression):
    def __init__(self, name): 
        self.name  = name    

    def __repr__(self) -> str:
        return self.name

    def eval(self):
        if self.name in storage:
            return storage[self.name]
        else:
            print(self.name)
            raise NameError(f"'{self.name}' is not defined")


class IfStatement(Statement):
    def __init__(self, condition, true_stmt: Statement, flase_stmt: Statement):
        self.condition = condition
        self.true_stmt = true_stmt
        self.flase_stmt = flase_stmt

    def __repr__(self) -> str:
        return f"(if ({self.condition}) ({self.true_stmt}) ({self.flase_stmt}))"

    def eval(self):
        if self.condition.eval():
            self.true_stmt.eval()  
        else:
            self.flase_stmt.eval()


class WhileStatement(Statement):
    def __init__(self, condition: Expression, stmt: Statement):
        self.condition = condition
        self.stmt = true_stmt
   
    def __repr__(self) -> str:
        return f"(while ({self.condition}) ({self.stmt}))"

    def eval(self):
        while self.condition.eval():
            self.stmt.eval() 


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
            "&&": lambda a,b: a and b,
            "||": lambda a,b: a or b,
        }
        return operator[self.operator](self.left.eval(), self.right.eval()) 


class PrefixExpression(Expression):
    def __init__(self, operator: Statement, right: Expression):
        self.operator = operator
        self.right = right

    def __repr__(self) -> str:
        return f"({self. operator}{self.right})"

    def eval(self):
        operator =  {
            "!" : lambda a: not a,
            "-" : lambda a: -a,
        }
        return operator[self.operator](self.right.eval()) 

class PrintStatement(Statement):
    def __init__(self, value: Expression):
        self.value = value

    def __repr__(self) -> str:
        return f"(print {self.value})"

    def eval(self):
        print(self.value.eval())









