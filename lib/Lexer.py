from Token import token, Token
from utils import (is_digit, is_whitespace, is_letter)

class Lexer(object):
    def __init__(self, input_data: str) -> None:
        self.input_data: str = input_data # Actual data
        self.pos: int        = 0          # Reading actual data
        self.read_pos: int   = 0          # For peeking into next char
        self.ch: str         = ''         # current char under examination

    def read_char(self) -> None:
        # it will read next charector and advance our position in input
        if self.read_pos >= len(self.input_data):
            # at the end of input set self.ch as 0 
            # 0 is "NUL" charector in ASCII 
            self.ch = 0
        else:
            self.ch = self.input_data[self.read_pos]
        self.pos = self.read_pos
        self.read_pos += 1

    def peek_char(self) -> None:
        if self.read_pos >= len(self.input_data):
            return  0
        else:
            return self.input_data[self.read_pos]
        

    def next_token(self) -> token:
        self.read_char()

        data = self.ch+self.peek_char()
        switcher_double = {
            "==": Token.EQUAL,
            "!=": Token.NOTEQ,
            "<=": Token.SMALLEQ,
            ">=": Token.LARGEEQ,
            ">>": Token.RSHIFT,
            "<<": Token.LSHIFT
        }
      
        switcher_single = {           
            "+": Token.PLUS,  
            "-": Token.MINUS, 
            "/": Token.DIVIDE,
            "*": Token.TIMES,
            "%": Token.MODULUS, 
            "^": Token.XOR,
            "=": Token.ASSIGN,
            "|": Token.OR,
            "&": Token.AND,
            ",": Token.COMMA,    
            ";": Token.SEMICOLON,  
            "(": Token.LPAREN,     
            ")": Token.RPAREN,     
            "{": Token.LBRACE,     
            "}": Token.RBRACE,     
            "[": Token.LSQUARE,    
            "]": Token.RSQUARE,    
            "!": Token.BANG,
             0 : Token.EOF, 
            
        }

        if ttype := switcher_double.get(data, None):
            # if the ttype is not None then we got the token
            self.read_char()
            return token(ttype,data)

        elif ttype := switcher_single.get(self.ch, None):
            # if the ttype is not None then we got the token
            return token(ttype, self.ch)
        
       
        elif is_letter(self.ch):
            pos = self.pos
            while is_letter(self.ch()): 
                self.read_char()
            data: str = self.input_data[pos: self.pos]
            return token(Token.STRING, data)

        elif is_digit(self.ch):
            pos = self.pos
            is_float: bool = False
            while is_digit(self.ch): 
                self.read_char()
                if self.ch == ".":
                    is_float = True
                    continue
            data: str = self.input_data[pos: self.pos]
            return token(Token.INT, int(data)) \
                    if ttype == Token.INT \
                    else token(Token.FLOAT, float(data))
        else: 
            return toekn(token.INVALID, self.ch)
        

if __name__ == "__main__":
    data: str = r">=+-*/=^,;()[]{}&|=="
    lexer: Lexer = Lexer(data)
    while tok := lexer.next_token():
        if tok.literal == 0:
            break
        print(tok)