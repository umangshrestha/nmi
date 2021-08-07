from parser import *
from lexer import *
from tokens import *
import pytest


@pytest.mark.parametrize("data, expected", [
    ("2+3*4", "(2 + (3 * 4))"),
    ("-1 +3/4 - 4", "((-1) + ((3 / 4) - 4))"),
    ("(1^  3)%3/3.0", "((1 ^ 3) % (3 / 3.0))")
])
def test_parser(data, expected):
    output = next(Parser(Lexer(data)))
    assert  output.__repr__() == expected

