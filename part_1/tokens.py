from enum import Enum
import re
from collections import namedtuple


TokenInfo = namedtuple("Tokens", ["name", "value"])


class NewEnum(Enum):
    """ I want enum class to automatically compare name 
    between two Enum or comparing string to the Enum. 
    But once __eq__ is defined in an object it requres 
    __hash__(self) to be implemented to make the Enum 
    usuable as items in hash collections. """   
    def __eq__(self, b) -> bool:
        """ When I do == between the Enum I want to check the name"""
        if isinstance(b, str):
            print(2)
            return self.name == b 
        else:
            return  self.name == b.name

    def __hash__(self):
        return id(self.name)



ILLEGAL = 'ILLEGAL'
EOF     = 'EOF'


class Token(NewEnum):
    # data types
    STRING  = re.compile(r'(\".*\")|(\'.*\')')
    FLOAT   = re.compile(r'\d+\.\d+')
    INT     = re.compile(r'\d+')
    # brackets
    LPAREN  = re.compile(r'\(')
    RPAREN  = re.compile(r'\)')
    LBRACE  = re.compile(r'\{')
    RBRACE  = re.compile(r'\}')
    LSQUARE = re.compile(r'\[')
    RSQUARE = re.compile(r'\]')
    # oeprators
    ASSIGN  = re.compile(r'\=')
    # athemetic operators
    PLUS    = re.compile(r'\+')
    MINUS   = re.compile(r'\-')
    TIMES   = re.compile(r'\*')
    DIVIDE  = re.compile(r'/')
    MODULUS = re.compile(r'%')
    POWER   = re.compile(r'\^')
    # logical operators
    AND     = re.compile(r'&')
    OR      = re.compile(r'\')
    NOT     = re.compile(r'!')
    # conditional operators
    EQUAL   = re.compile(r'\=\=')
    SMALL   = re.compile(r'<')
    SMALLEQ = re.compile(r'<\=')
    LARGE   = re.compile(r'>')
    LARGEEQ = re.compile(r'>\=')
    NOTEQ   = re.compile(r'!\=')
    # keywords
    IF      = re.compile(r'if')
    WHILE   = re.compile(r'while')
    TRUE    = re.compile(r'true')
    FALSE   = re.compile(r'false')
    FUNC    = re.compile(r'func')
    ELSE    = re.compile(r'else')
    NAN     = re.compile(r'nan')
    LET     = re.compile(r'let')
    PRINT   = re.compile(r'(println)|(print)')
    RETURN  = re.compile(r'return')
    # variables
    ID      = re.compile(r'[_a-zA-Z][_a-zA-Z0-9]*')
    # comments
    COMMENT = re.compile(r'#.*')
    # delimier 
    COMMA   = re.compile(r',')
    SEMICOLON  = re.compile(r';')
    WHITESPACE = re.compile(r'(\t|\n|\s|\r)+')

