from enum import Enum
from typing import List, Dict
from tokens import Token


Priority = Enum("priority", [
    "LOWEST",       
    "EQUALS",       #  ==    
    "LESSGREATER",  #  < | >
    "SUM",          #  + | -
    "PRODUCT",      #  * | /
    "PREFIX",       # -1 | +1
    "CALL"          # function(x)
])

precedence: Dict[Token, Priority] = {
    Token.EQUAL : Priority.LOWEST,       # ==
    Token.NOTEQ : Priority.LOWEST,       # !=
    Token.SMALL : Priority.LESSGREATER,  # <  
    Token.LARGE : Priority.LESSGREATER,  # >
    Token.PLUS  : Priority.SUM,          # +
    Token.MINUS : Priority.SUM,          # -
    Token.TIMES : Priority.PRODUCT,      # *
    Token.DIVIDE: Priority.PRODUCT,      # /
    Token.LPAREN: Priority.CALL,         # ()  
}

def get_precedence(token: Token) -> Priority:
    return precedence.get(token, Priority.LOWEST)

