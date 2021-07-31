from lexer import Lexer
from tokens import Token, TokenInfo


def test_lexer():

    data: str = \
"""
let square = func(n) {
    return n * 2;
};
let data = 3.0;
let out = square(data);
print(out);
"""
    lexer: Lexer = Lexer(data)
    tests = [
        TokenInfo(type=Token.LET, literal="let", pos=1, line=2) ,
        TokenInfo(type=Token.ID, literal="square", pos=5, line=2) ,
        TokenInfo(type=Token.ASSIGN, literal="=", pos=12, line=2) ,
        TokenInfo(type=Token.FUNCTION, literal="func", pos=14, line=2) ,
        TokenInfo(type=Token.LPAREN, literal="(", pos=18, line=2) ,
        TokenInfo(type=Token.ID, literal="n", pos=19, line=2) ,
        TokenInfo(type=Token.RPAREN, literal=")", pos=20, line=2) ,
        TokenInfo(type=Token.LBRACE, literal="{", pos=22, line=2) ,
        TokenInfo(type=Token.RETURN, literal="return", pos=5, line=3) ,
        TokenInfo(type=Token.ID, literal="n", pos=12, line=3) ,
        TokenInfo(type=Token.TIMES, literal="*", pos=14, line=3) ,
        TokenInfo(type=Token.INT, literal="2", pos=16, line=3) ,
        TokenInfo(type=Token.SEMICOLON, literal=";", pos=17, line=3) ,
        TokenInfo(type=Token.RBRACE, literal="}", pos=1, line=4) ,
        TokenInfo(type=Token.SEMICOLON, literal=";", pos=2, line=4) ,
        TokenInfo(type=Token.LET, literal="let", pos=1, line=5) ,
        TokenInfo(type=Token.ID, literal="data", pos=5, line=5) ,
        TokenInfo(type=Token.ASSIGN, literal="=", pos=10, line=5) ,
        TokenInfo(type=Token.FLOAT, literal="3.0", pos=12, line=5) ,
        TokenInfo(type=Token.SEMICOLON, literal=";", pos=15, line=5) ,
        TokenInfo(type=Token.LET, literal="let", pos=1, line=6) ,
        TokenInfo(type=Token.ID, literal="out", pos=5, line=6) ,
        TokenInfo(type=Token.ASSIGN, literal="=", pos=9, line=6) ,
        TokenInfo(type=Token.ID, literal="square", pos=11, line=6) ,
        TokenInfo(type=Token.LPAREN, literal="(", pos=17, line=6) ,
        TokenInfo(type=Token.ID, literal="data", pos=18, line=6) ,
        TokenInfo(type=Token.RPAREN, literal=")", pos=22, line=6) ,
        TokenInfo(type=Token.SEMICOLON, literal=";", pos=23, line=6) ,
        TokenInfo(type=Token.ID, literal="print", pos=1, line=7) ,
        TokenInfo(type=Token.LPAREN, literal="(", pos=6, line=7) ,
        TokenInfo(type=Token.ID, literal="out", pos=7, line=7) ,
        TokenInfo(type=Token.RPAREN, literal=")", pos=10, line=7) ,
        TokenInfo(type=Token.SEMICOLON, literal=";", pos=11, line=7) ,
    ]
    for expected in tests:
        output = lexer.next_token()
        print(output)
        if output.type != expected.type:
            assert False, f"type error: expected: {expected.type}, Received:{output.type}" 
        if output.literal != expected.literal:
            assert False, f"literal error: expected: {expected.literal}, Received:{output.literal}" 
        if output.pos != expected.pos:
            assert False, f"literal error: expected: {expected.pos}, Received:{output.pos}" 
        

