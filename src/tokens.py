from collections import namedtuple
from typing import Dict
from enum import Enum

class TokenInfo(namedtuple('TokenInfo', ["type", "literal", "pos", "line"])):
    """ Token info consists of: 
    1. type:  The type that token is classified under. It is defined under Token enum.
    2. value: It  the actual value  of token.
    3. lineno: It stores line number.
    4. pos: Position of value in the line."""
    def __repr__(self):
        """ Changing printing format:
        default print: TokenInfo(type=<Token.ID: 35>, , pos=1, line=2)
        updated print: (type=Token.ID, literal="b", , pos=1, line=2)"""
        return f"(type={self.type}, literal=\"{self.literal}\", pos={self.pos}, line={self.line})"
    

Token = Enum("Token", [
    "ASSIGN",    # "="
##############
# Arthemetic operators
##############
    "PLUS",      # "+"
    "MINUS",     # "-"
    "DIVIDE",    # "/"
    "TIMES",     # "*"
    "MODULUS",   # "%"
##############
# Logical operators 
#############
    "AND",       # "&"
    "OR",        # "|"
    "XOR",       # "^"
    "NOT",       # "!"
##############
# Comparisions
##############
    "EQUAL",     # "=="
    "NOTEQ",     # "!="
    "SMALL",     # "<"
    "LARGE",     # ">"
    "SMALLEQ",   # "<="
    "LARGEEQ",   # ">="
##############
# Brackets
##############
    "COMMA",     # ","
    "SEMICOLON", # ";"
    "LPAREN",    # "("
    "RPAREN",    # ")"
    "LBRACE",    # "{"
    "RBRACE",    # "}"
    "LSQUARE",   # "["
    "RSQUARE",   # "]"
##############
# EXTRA
##############
    "ID",        #  Variables
    "EOF",       #  End of line
    "ILLEGAL",   #  Symbol not known
##############
# Datatypes
##############  
    "INT",       
    "FLOAT",     
    "STRING",    
    "TRUE",      
    "FALSE",     
##############
# Keywords
##############
    "FUNCTION",  
    "LET",       
    "IF",        
    "ELSE",      
    "RETURN",    
    "PRINT"
])


keywords: Dict[str, Token] = {
    "func"  : Token.FUNCTION,
    "let"   : Token.LET,
    "if"    : Token.IF,
    "else"  : Token.ELSE,
    "true"  : Token.TRUE,
    "false" : Token.FALSE,
    "return": Token.RETURN,
    "print" : Token.PRINT,
}

def get_token_for_keyword(data: str) -> Token:
    # checks if the given string is keyword or not
    # if its a keyword return the respective Token
    # or by default return Token.Id 
    return keywords.get(data, Token.ID)


