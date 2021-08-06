
from tokens import *
    
    
def Lexer(data: str) -> tokenInfo:
    pos = 0 
    while pos < len(data):
        for tokenId in Token:
            if match := tokenId.value.match(data, pos):
                pos = match.end(0)
                yield tokenInfo(tokenId.name, match.group(0))
                break
        else:
            yield tokenInfo(ILLEGAL, data[pos])
    else:
        # in parser we read the token two times for a given operation
        # once for current token and one for next token
        # so in case its read two times handling the EOF
        yield tokenInfo(EOF, '\x00') 
        yield tokenInfo(EOF, '\x00') 


        