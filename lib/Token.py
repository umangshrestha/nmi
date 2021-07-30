from collections import namedtuple
from typing import Dict
from enum import Enum

token = namedtuple("Token", [
    "type",   # str: Diffrentiate between 'integer' and 'semicolon'
    "literal",# str : Actual value of like 12 in 'integer'
])

class Token(Enum):
    # operators
    PLUS    = 1
    MINUS   = 2
    DIVIDE  = 3
    TIMES   = 4
    MODULUS = 5
    ASSIGN  = 6
    AND     = 9
    OR      = 11
    XOR     = 12
    NOT     = 14
    LSHIFT  = 15
    RSHIFT   = 16
    BANG    = 17
    # compare
    EQUAL   = 50
    NOTEQ   = 51
    SMALL   = 52
    LARGE   = 53
    SMALLEQ = 54
    LARGEEQ = 55



    # delimiters
    COMMA     = 100
    SEMICOLON = 101
    LPAREN    = 102
    RPAREN    = 102
    LBRACE    = 104
    RBRACE    = 105
    LSQUARE   = 106
    RSQUARE   = 107
    INDENT    = 108
    ILLEGAL   = 109
    EOF       = 110
    # datatypes
    INT    = 200
    FLOAT  = 201
    STRING = 202
    # keywords
    FUNCTION = 300
    LET      = 301
    IF       = 302
    ELSE     = 303
    RETURN   = 304
    TRUE     = 303
    FALSE    = 304


    def __repr__(self):
        return self.name



keywords: Dict[str, Token] = {
    "func" : Token.FUNCTION,
    "let"  : Token.LET,
    "if"   : Token.IF,
    "else" : Token.ELSE,
    "true" : Token.TRUE,
    "false": Token.FALSE,
    "return": Token.RETURN
}

