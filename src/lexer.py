from tokens import TokenInfo, Token, get_token_for_keyword

class Lexer(object):
    def __init__(self, input_data: str) -> None:
        self.input: str = input_data      # Actual data
        self.pos: int        = 0          # position from refrence to line
        self.ch: str         = ""         # current char under examination
        # Optional data
        # added to help for debugging
        self.lineno: int     = 1          # line number of data
        self.linepos: int    = 1          # position from refrence to line
        self.read_char()                      
    
    def read_char(self) -> None:
        # reading value of next charector 
        # modifying the  variables:
        #        self.ch, self.pos, self.linepos
        # \x00 is send at end 
        if self.pos >= len(self.input):
            self.ch = "\x00"
        else:
            self.ch = self.input[self.pos]
        self.pos += 1
        self.linepos +=1

    def peek_char(self) -> str:
        # reading value of next charector 
        # dont modifying the variables:
        if self.pos >= len(self.input):
            return  "\x00"
        else:
            return self.input[self.pos]
        

    def next_token(self) -> TokenInfo:
        # escape all the whitespaces
        while self.is_whitespace():
            if self.ch == "\n":
                # if new line then update
                # line number and reset line pos
                self.lineno += 1
                self.linepos = 1
            self.read_char()

      
        if self.peek_char()  != 0  and self.peek_char()  != " " :
            # checking for double charectors like '==', '>=', '!='
            # if next charector exits  then check if the patttern exits
            data = self.ch+self.peek_char()
            switcher = {
                "==": TokenInfo(Token.EQUAL, data, self.linepos, self.lineno),
                "!=": TokenInfo(Token.NOTEQ, data, self.linepos, self.lineno),
                "<=": TokenInfo(Token.SMALLEQ, data, self.linepos, self.lineno),
                ">=": TokenInfo(Token.LARGEEQ, data, self.linepos, self.lineno),
            }

            if tok := switcher.get(data, None):             
                # if pattern exits then return 
                self.read_char() # compensating for peek
                self.read_char() # going to next char
                return tok

        switcher = {
            "+": TokenInfo(Token.PLUS,   self.ch, self.linepos-1, self.lineno),  
            "-": TokenInfo(Token.MINUS,  self.ch, self.linepos-1, self.lineno), 
            "/": TokenInfo(Token.DIVIDE, self.ch, self.linepos-1, self.lineno),
            "*": TokenInfo(Token.TIMES,  self.ch, self.linepos-1, self.lineno),
            "%": TokenInfo(Token.MODULUS,self.ch, self.linepos-1, self.lineno), 
            "^": TokenInfo(Token.XOR,    self.ch, self.linepos-1, self.lineno),
            "=": TokenInfo(Token.ASSIGN, self.ch, self.linepos-1, self.lineno),
            "|": TokenInfo(Token.OR,     self.ch, self.linepos-1, self.lineno),
            "&": TokenInfo(Token.AND,    self.ch, self.linepos-1, self.lineno),
            ",": TokenInfo(Token.COMMA,  self.ch, self.linepos-1, self.lineno),    
            ";": TokenInfo(Token.SEMICOLON,self.ch, self.linepos-1, self.lineno),  
            "(": TokenInfo(Token.LPAREN, self.ch, self.linepos-1, self.lineno),     
            ")": TokenInfo(Token.RPAREN, self.ch, self.linepos-1, self.lineno),     
            "{": TokenInfo(Token.LBRACE, self.ch, self.linepos-1, self.lineno),     
            "}": TokenInfo(Token.RBRACE, self.ch, self.linepos-1, self.lineno),     
            "[": TokenInfo(Token.LSQUARE,self.ch, self.linepos-1, self.lineno),    
            "]": TokenInfo(Token.RSQUARE,self.ch, self.linepos-1, self.lineno),  
            "<": TokenInfo(Token.SMALL,  self.ch, self.linepos-1, self.lineno),   
            ">": TokenInfo(Token.LARGE,  self.ch, self.linepos-1, self.lineno),   
            "!": TokenInfo(Token.NOT,    self.ch, self.linepos-1, self.lineno),
            "\x00" : TokenInfo(Token.EOF,self.ch, self.linepos-1, self.lineno), 
            
        }
            
        if tok := switcher.get(self.ch, None): 
            # checking for single charectos like '+', '-', '*'
            # if the ttype is not None then we got the token
            self.read_char() # going to next char
            return tok
        
        elif self.is_letter():
            # checking string charectors
            # finding the start and end pos of string
            pos = self.pos 
            linepos = self.linepos -1
            while self.is_letter():  
                self.read_char()
            data: str = self.input[pos-1: self.pos-1]
            # check if string is keyword
            ttype = get_token_for_keyword(data)
            return TokenInfo(ttype, data, linepos, self.lineno) 
            
        elif self.is_digit():
            pos = self.pos 
            linepos = self.linepos -1
            is_float: bool = False
            while self.is_digit(): 
                self.read_char()
                if self.ch == ".":
                    is_float = True
                    continue
            data: str = self.input[pos-1: self.pos - 1] 
            return TokenInfo(Token.INT if is_float == False else Token.FLOAT , data, linepos, self.lineno) 
           
        else:
            tok = TokenInfo(Token.ILLEGAL, self.ch, self.linepos, self.lineno)
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
        return ("0" <= self.ch <= "9") \
                | (self.ch == ".")
        