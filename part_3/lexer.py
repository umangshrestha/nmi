from tokens import *
    
___all__ = ["Lexer"]

def Lexer(data: str) -> TokenInfo:
    pos = 0 
    while pos < len(data):
        for tokenId in Token:
            if match := tokenId.value.match(data, pos):
                pos = match.end(0)
                if tokenId == Token.WHITESPACE or tokenId == Token.COMMENT:
                    # ignore white spaces or comments 
                    break
                yield TokenInfo(tokenId.name, match.group(0))
                break
        else:
            # in case pattern doesn't match send the charector as illegal
            yield TokenInfo(ILLEGAL, data[pos])
            pos += 1
    else:
        # in parser we read the token two times each iteration
        # once for current token and one for next token
        # so handing it by sending EOF 2 times
        yield TokenInfo(EOF, '\x00') 
        yield TokenInfo(EOF, '\x00') 
       