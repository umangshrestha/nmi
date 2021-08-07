from tokens import *
from lexer import Lexer
import pytest

class TestLegalData:
    def setup_class(self):
        data: str = """
        let square = func(n) {
            return n * 2;
        };
        let data = 3.0;
        let out = square(data);
        print(out);
        """
        self.lexer = Lexer(data)
    
    @pytest.mark.parametrize("token, value", [
        (Token.LET, "let"),
        (Token.ID, "square"),
        (Token.ASSIGN, "="),
        (Token.FUNC, "func"),
        (Token.LPAREN, "("),
        (Token.ID, "n"),
        (Token.RPAREN, ")"),
        (Token.LBRACE, "{"),
        (Token.RETURN, "return"),
        (Token.ID, "n"),
        (Token.TIMES, "*"),
        (Token.INT, "2"),
        (Token.SEMICOLON, ";"),
        (Token.RBRACE, "}"),
        (Token.SEMICOLON, ";"),
        (Token.LET, "let"),
        (Token.ID, "data"),
        (Token.ASSIGN, "="),
        (Token.FLOAT, "3.0"),
        (Token.SEMICOLON, ";"),
        (Token.LET, "let"),
        (Token.ID, "out"),
        (Token.ASSIGN, "="),
        (Token.ID, "square"),
        (Token.LPAREN, "("),
        (Token.ID, "data"),
        (Token.RPAREN, ")"),
        (Token.SEMICOLON, ";"),
        (Token.PRINT, "print"),
        (Token.LPAREN, "("),
        (Token.ID, "out"),
        (Token.RPAREN, ")"),
        (Token.SEMICOLON, ";"),
    ])
    def test_lexer(self, token, value):
        expected = TokenInfo(token.name, value)
        curr_token = next(self.lexer)
        
        if curr_token != expected:
            assert False, f"name error: expected: {expected.name}, Received:{curr_token.name}" 
        if curr_token.value != expected.value:
            assert False, f"value error: expected: {expected.value}, Received:{curr_token.value}" 

class TestIllegalData:
    def setup_class(self):
        data: str = "~?:"
        self.lexer = Lexer(data)
    
    @pytest.mark.parametrize("token, value", [
        (ILLEGAL, "~"),
        (ILLEGAL, "?"),
        (ILLEGAL, ":"),
        (EOF, "\x00"),
        (EOF, "\x00"),
    ])
    def test_illegal_lexer(self, token, value):
        expected = TokenInfo(token, value)
        curr_token = next(self.lexer)
        
        if curr_token != expected:
            assert False, f"name error: expected: {expected.name}, Received:{curr_token.name}" 
        if curr_token.value != expected.value:
            assert False, f"value error: expected: {expected.value}, Received:{curr_token.value}" 