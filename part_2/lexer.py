from tokens import *


class Lexer(object):
    def __init__(self, input_data: str) -> None:
        self.input: str = input_data # Actual data
        self.pos: int   = 0          # position from refrence to line
        self.ch: str    = ""         # current char under examination
        self.read_char()                      
    
    def read_char(self) -> None:
        # update self.ch and self.pos
        if self.pos >= len(self.input):
            self.ch  = "\x00"
        else:
            self.ch  = self.input[self.pos]
        self.pos += 1

    def __iter__(self):
        return self

    def __next__(self) -> TokenInfo:
        return self.lex_eol() \
            or self.remove_whitespace() \
            or self.remove_comments()  \
            or self.lex_operator()  \
              or self.lex_string()  \
              or self.lex_letters() \
              or self.lex_integer() \
              or self.lex_illegal()

    def lex_eol(self) -> TokenInfo:
         if self.ch == '\x00':
            return TokenInfo(EOF, self.ch)
       
    def remove_whitespace(self) -> None:
        while is_whitespace(self.ch):
            self.read_char()

    def remove_comments(self) -> None:
        if self.ch == '#"':
            self.read_char()
            while self.ch not in ["\n", "\r", "\x00"]:
                 self.read_char()

    def lex_operator(self) -> TokenInfo:
        switcher = {
            "+": Token.PLUS,    "-": Token.MINUS,  "/": Token.DIVIDE,  "*": Token.TIMES,
            "%": Token.MODULUS, "^": Token.POWER,  "=": Token.ASSIGN,  "!": Token.NOT,
            "|": Token.OR,      "&": Token.AND,    ",": Token.COMMA,   ";": Token.SEMICOLON,  
            "(": Token.LPAREN,  ")": Token.RPAREN, "{": Token.LBRACE,  "}": Token.RBRACE,     
            "[": Token.LSQUARE, "]": Token.RSQUARE, "<": Token.SMALL,  ">": Token.LARGE,   
            
        }
        if tok := switcher.get(self.ch, None): 
            # checking for single charectors like '+', '-', '*'
            ch = self.ch
            self.read_char()
            double_switcher = {
                ">=": Token.LARGEEQ,
                "<=": Token.SMALLEQ,
                "==": Token.EQUAL,
                "!=": Token.NOTEQ,
            }
            # checking for double charectors like '>', '<=', '!=' 
            if new_tok := double_switcher.get(ch+self.ch, None):
                tok = new_tok
                ch += self.ch
                self.read_char()
            return TokenInfo(tok.name, ch)
    
    def lex_string(self) -> TokenInfo:
        if self.ch == '\"':
            pos = self.pos 
            self.read_char()
            while self.ch != '\"':
                if self.ch in  ["\n", "\r", "\x00"]:
                    raise SyntaxError("string literal not closed")
                self.read_char()
            data: str = self.input[pos: self.pos]
            self.read_char()
            return TokenInfo(Token.STRING.name, data) 

    def lex_letters(self) -> TokenInfo:
        if is_letter(self.ch):
            pos = self.pos 
            while is_letter(self.ch):  
                self.read_char()
            data: str = self.input[pos-1: self.pos-1]
            ttype = get_token(data)
            return TokenInfo(ttype, data) 

    def lex_integer(self) -> TokenInfo:
        # checking for float and integer
        if is_digit(self.ch):
            pos = self.pos 
            ttype = Token.INT.name
            while is_digit(self.ch): 
                self.read_char()
                if self.ch == ".":
                    ttype = Token.FLOAT.name
                    continue
            data: str = self.input[pos-1: self.pos - 1] 
            return TokenInfo(ttype , data) 

    def lex_illegal(self):  
        tok = TokenInfo(ILLEGAL, self.ch)
        self.read_char()
        return tok
    

def is_letter(char: str) -> bool:
    "Checking if given self.ch is letter"
    return  ("a" <= char <="z")   \
            | ("A" <= char <="Z") \
            | ('_' == char )


def is_whitespace(char: str) -> bool:
    "Checking if the given input is whitespace."
    return  (char == " ")     \
            | (char == "\t") \
            | (char == "\n") \
            | (char == "\r") 

def is_digit(char: str) -> bool:
    "Checking if the given input is digit"
    return ("0" <=  char <= "9") \
            | (char == ".")
    
if __name__ == "__main__":
    data: str = """
        let square = func(n) {
            return n * 2;
        };
        let data = 3.0;
        let out = square(data);
        print(out);
    """

    for i in Lexer(data):
        print(1, i)
        if i.name == EOF:
            exit(0)