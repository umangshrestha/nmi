from lexer import Lexer
from tokens import Token, TokenInfo


def test_lexer():

    data: str = r"""<= !+-*/=^,;()[]{}&|="""
    lexer: Lexer = Lexer(data)
    tests = [
        (Token.SMALLEQ,    "<=", 0, 2 ),
        (Token.NOT,        "+", 0 ,2),
        (Token.PLUS,       "+",0 ,2),
        (Token.MINUS,      "-", 0 ,2),
        (Token.TIMES,      "*", 0 ,2),
        (Token.DIVIDE,     "/", 0 ,2),
        (Token.ASSIGN,     "=", 0 ,2),
        (Token.XOR,        "^", 0 ,2),
        (Token.COMMA,      ",", 0 ,2),
        (Token.SEMICOLON,  ";", 0 ,2),
        (Token.LPAREN,     "(", 0 ,2),
        (Token.RPAREN,     ")", 0 ,2),
        (Token.LSQUARE,    "[", 0 ,2),
        (Token.RSQUARE,    "]", 0 ,2),
        (Token.LBRACE,     "{", 0 ,2),
        (Token.RBRACE,     "}", 0 ,2),
        (Token.AND,        "&", 0 ,2),
        (Token.OR,         "|", 0 ,2),
    ]
    for test in tests:
        expected = TokenInfo(*test)
        output = lexer.next_token()
        print(output)
        if output.type != expected.type:
            assert False, f"type error: expected: {expected.type}, Received:{output.type}" 
        if output.literal != expected.literal:
            assert False, f"literal error: expected: {expected.literal}, Received:{output.literal}" 
        if output.literal != expected.literal:
            assert False, f"literal error: expected: {expected.literal}, Received:{output.literal}" 
        