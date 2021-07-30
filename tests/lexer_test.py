import pytest
from Lexer import Lexer
from Token import Token, token

class TestLexer:
    @classmethod
    def setup_class(self):
        data: str = r">=+-*/=^,;()[]{}&|=="
        self.lexer: Lexer = Lexer(data)

    @pytest.mark.parametrize("tests", [
        (Token.LARGEEQ,    ">="),
        (Token.PLUS,       "+"),
        (Token.MINUS,      "-"),
        (Token.TIMES,      "*"),
        (Token.DIVIDE,     "/"),
        (Token.ASSIGN,     "="),
        (Token.XOR,        "^"),
        (Token.COMMA,      ","),
        (Token.SEMICOLON,  ";"),
        (Token.LPAREN,     "("),
        (Token.RPAREN,     ")"),
        (Token.LSQUARE,    "["),
        (Token.RSQUARE,    "]"),
        (Token.LBRACE,     "{"),
        (Token.RBRACE,     "}"),
        (Token.AND,        "&"),
        (Token.OR,         "|"),
        (Token.EQUAL,     "=="),
    ])
    def test_lexer(self, tests):
        expected = token(*tests)
        output = self.lexer.next_token()
        if output.type != expected.type:
            assert False, f"Type error: expected: {expected.type}, Received:{output.type}" 
        if output.literal != expected.literal:
            assert False, f"Literal error: expected: {expected.literal}, Received:{output.literal}" 






