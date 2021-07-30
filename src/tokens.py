from collections import namedtuple
from types import SimpleNamespace
from enum import Enum

class TokenInfo(namedtuple('TokenInfo', ["type", "literal", "pos", "line"])):
    """ Token info consists of: 
    1. type:  It helps us diffrentiate between diffrent token types like 'integers' or 'semicolon'.
    2. value: It  the actual value  of token.
    """
    def __repr__(self):
        """ Changing printing format:
        default print: TokenInfo(type=<Token.ID: 35>, literal=2)
        updated print: TokenInfo(type=Token.ID, literal=2)"""
        return f"{self.__class__.__name__}(type={self.type}, literal=\"{self.literal}\", pos={self.pos}, line={self.line})"
    

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
    "PLUSEQ",    # "+="
    "MINUSEQ",   # "-="
    "TIMESEQ",   # "*="
    "DIVIDEEQ",  # "/="
    "MODULUSEQ", # "%="
##############
# Logical operators 
#############
    "AND",       # "&"
    "OR",        # "|"
    "XOR",       # "^"
    "NOT",       # "!"
    "ANDEQ",     # "&="
    "OREQ",      # "|="
    "XOREQ",     # "^="
    "LSHIFT",    # "<<"
    "RSHIFT",    # ">>"
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
    "EOL",       #  End of line
    "ILLEGAL",   # Symbol not known
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
])



keywords = {
    "func"  : Token.FUNCTION,
    "let"   : Token.LET,
    "if"    : Token.IF,
    "else"  : Token.ELSE,
    "true"  : Token.TRUE,
    "false" : Token.FALSE,
    "return": Token.RETURN
}

