from tokens import TokenInfo
from typing import List



__all__ = ["Expression", "Statement", "Program"]


class Node(object):   
    def get_literal(self)->TokenInfo.literal:
        pass


class Expression(Node): pass
    

class Statement(Node):  pass


class Program(Node):
    statements: Statement = list()

    def get_literal(self) -> str:
        if len(self.statements) >0:
            return self.statements[0].get_literal()
        else:
            return ""
