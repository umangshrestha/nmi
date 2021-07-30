from tokens import TokenInfo, Token, keywords

class Lexer(object):
    def __init__(self, input_data: str) -> None:
        self.input: str = input_data # Actual data
        self.pos: int        = 0          # Reading actual data
        self.ch: str         = ""         # current char under examination
        self.lineno: int     = 0
        self.pos: int        = 0
        self.read_char()
    
    def read_char(self) -> None:
        if self.pos >= len(self.input):
            self.ch = "\x00"
        else:
            self.ch = self.input[self.pos]
        self.pos += 1

    def peek_char(self) -> None:
        if self.pos+1 >= len(self.input):
            return  0
        else:
            return self.input[self.pos+1]
        

    def next_token(self) -> TokenInfo:
        # escape all the whitespaces
    
        while self.is_whitespace():
            if self.ch == "\n":
                    self.lineno += 1
            self.read_char()
        if self.peek_char() != 0 :
            # checking for double charectors like '==', '>=', '!='
            # if next charector exits  then check if the patttern exits
            data = self.ch+self.peek_char()
            switcher = {
                "==": TokenInfo(Token.EQUAL, data, self.pos, self.lineno),
                "!=": TokenInfo(Token.NOTEQ, data, self.pos, self.lineno),
                "<=": TokenInfo(Token.SMALLEQ, data, self.pos, self.lineno),
                ">=": TokenInfo(Token.LARGEEQ, data, self.pos, self.lineno),
                ">>": TokenInfo(Token.RSHIFT, data, self.pos, self.lineno),
                "<<": TokenInfo(Token.LSHIFT, data, self.pos, self.lineno), 
                "+=": TokenInfo(Token.PLUSEQ, data, self.pos, self.lineno),
                "-=": TokenInfo(Token.MINUSEQ, data, self.pos, self.lineno),
                "*=": TokenInfo(Token.TIMESEQ, data, self.pos, self.lineno),
                "/=": TokenInfo(Token.DIVIDEEQ, data, self.pos, self.lineno),
                "%=": TokenInfo(Token.MODULUSEQ, data, self.pos, self.lineno),
            }

            if tok := switcher.get(data, None):
                # if pattern exits then return 
                self.read_char() # compensating for peek
                self.read_char() # going to next char
                return tok

        switcher = {
            "+": TokenInfo(Token.PLUS, self.ch, self.pos-1, self.lineno),  
            "-": TokenInfo(Token.MINUS, self.ch, self.pos-1, self.lineno), 
            "/": TokenInfo(Token.DIVIDE, self.ch, self.pos-1, self.lineno),
            "*": TokenInfo(Token.TIMES, self.ch, self.pos-1, self.lineno),
            "%": TokenInfo(Token.MODULUS, self.ch, self.pos-1, self.lineno), 
            "^": TokenInfo(Token.XOR, self.ch, self.pos-1, self.lineno),
            "=": TokenInfo(Token.ASSIGN, self.ch, self.pos-1, self.lineno),
            "|": TokenInfo(Token.OR, self.ch, self.pos-1, self.lineno),
            "&": TokenInfo(Token.AND, self.ch, self.pos-1, self.lineno),
            ",": TokenInfo(Token.COMMA, self.ch, self.pos-1, self.lineno),    
            ";": TokenInfo(Token.SEMICOLON, self.ch, self.pos-1, self.lineno),  
            "(": TokenInfo(Token.LPAREN, self.ch, self.pos-1, self.lineno),     
            ")": TokenInfo(Token.RPAREN, self.ch, self.pos-1, self.lineno),     
            "{": TokenInfo(Token.LBRACE, self.ch, self.pos-1, self.lineno),     
            "}": TokenInfo(Token.RBRACE, self.ch, self.pos-1, self.lineno),     
            "[": TokenInfo(Token.LSQUARE, self.ch, self.pos-1, self.lineno),    
            "]": TokenInfo(Token.RSQUARE, self.ch, self.pos-1, self.lineno),  
            "<": TokenInfo(Token.SMALL, self.ch, self.pos-1, self.lineno),   
            ">": TokenInfo(Token.LARGE, self.ch, self.pos-1, self.lineno),   
            "!": TokenInfo(Token.NOT, self.ch, self.pos-1, self.lineno),
            "\x00" : TokenInfo(Token.EOL, self.ch, self.pos-1, self.lineno), 
            
        }
            
        if tok := switcher.get(self.ch, None): 
            # checking for single charectos like '+', '-', '*'
            # if the ttype is not None then we got the token
            self.read_char() # going to next char
            return tok
        
        elif self.is_letter():
            # checking string charectors
            # finding the start and end pos of string
            pos = self.pos - 1
            while self.is_letter():  self.read_char()
            data: str = self.input[pos: self.pos-1]
            # check if string is keyword
            if  ttype := keywords.get(data, None):
                return TokenInfo(ttype, data, pos, self.lineno) 
            else:
                return TokenInfo(Token.ID, data, pos, self.lineno)

        elif self.is_digit():
            pos = self.pos - 1
            is_float: bool = False
            while self.is_digit(): 
                self.read_char()
                if self.ch == ".":
                    is_float = True
                    continue
            data: str = self.input[pos-1: self.pos - 1]
            if is_float == False: 
                return TokenInfo(Token.INT, data, pos, self.lineno) 
            else:
                return TokenInfo(Token.FLOAT, data, pos, self.lineno)
        else:
            tok = TokenInfo(Token.ILLEGAL, self.ch, self.pos, self.lineno)
            self.read_char()
            return tok

    def is_letter(self) -> bool:
        "Checking if given self.ch is letter"
        return  ("a" <= self.ch <="z")   \
                | ("A" <= self.ch <="Z") \
                | ('_' == self.ch )


    def is_whitespace(self) -> bool:
        "Checking if the given self.ch is whitespace."
        return  (self.ch == " ")     \
                | (self.ch == "\t") \
                | (self.ch == "\n") \
                | (self.ch == "\r") 

    def is_digit(self) -> bool:
        "Checking if the given self.ch is digit"
        return "0" <= self.ch <= "9"
        

if __name__ == "__main__":
    data: str = "cc=1+1"
    lexer: Lexer = Lexer(data)
    while tok := lexer.next_token():
        if tok.type == Token.EOL:
            break
        print(tok)