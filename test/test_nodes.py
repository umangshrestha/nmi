import pytest
from nodes import *
from tokens import *

@pytest.mark.parametrize("data, expected", [
    ((Token.INT.name, "3"),      ("int", "3", 3)),
    ((Token.FALSE.name,"false"), ("bool", "false", False)),
    ((Token.TRUE.name,"true"),   ("bool", "true", True)),
    ((Token.STRING.name,"true"),   ("string", "true", "true")),
    ((Token.FLOAT.name, "3.0"),   ("float", "3.0",3.0)),
])
def test_literal(data, expected):
    received = Literal(*data)
    typ, string, value = expected
    if typ != received.get_type():
        assert False, f"name error: expected: {typ}, Received:{received.get_type()}" 
    if value != received.eval():
        assert False, f"value error: expected: {value}, Received:{received.eval()}" 
    if string != received.__repr__():
        assert False, f"value error: expected: {string}, Received:{received}"


@pytest.mark.parametrize("data, expected", [
    (
        (Literal(Token.INT.name, "4"), "*" , Literal(Token.INT.name, "1")), 
        ("(4 * 1)", 4)
    ),(
        (Literal(Token.INT.name, "3"), "-", Literal(Token.FLOAT.name, "10.0")), 
        ("(3 - 10.0)", -7.0)
    ),(
        (Literal(Token.TRUE.name, "true"), "+", Literal(Token.TRUE.name, "true")), 
        ("(true + true)", 2)
    ),(
        (Literal(Token.STRING.name, "Apple"), "+", Literal(Token.STRING.name, "Banana")), 
        ("(Apple + Banana)", "AppleBanana")
    ),
])
def test_infix(data, expected):
    received = InfixExpression(*data)
    string, value = expected
    if value != received.eval():
        assert False, f"value error: expected: {value}, Received:{received.eval()}" 
    if string != received.__repr__():
        assert False, f"value error: expected: {string}, Received:{received}"


@pytest.mark.parametrize("data, expected", [
    (
        ("-", Literal(Token.INT.name, "3")),
        ("(-3)", -3)
    ), (
        ("!", Literal(Token.FALSE.name, "false")),
        ("(!false)", True)
    ), (
        ("-", Literal(Token.TRUE.name, "true")),
        ("(-true)", -1)
    ), (
        ("!", Literal(Token.INT.name, "1")),
        ("(!1)", False)
    ),
])
def test_prefix(data, expected):
    received = PrefixExpression(*data)
    string, value = expected
    if value != received.eval():
        assert False, f"value error: expected: {value}, Received:{received.eval()}" 
    if string != received.__repr__():
        assert False, f"value error: expected: {string}, Received:{received}"


@pytest.mark.parametrize("data, expected", [
    (Identifier("a"), "a"),
    (LetStatement(Identifier("a"), Literal(Token.INT.name, 2)), "(let a 2)"),
    (AssignStatement(Identifier("a"), 4), "(set a 4)"),
    (PrintStatement("PRINT", "apple"), "(print apple)"),
    (PrintStatement("PRINTLN", 1230), "(println 1230)"),

])
def test_assignment(data, expected):
    if data.__repr__() != expected:
        assert False, f"value error: expected: {expected}, Received:{data}"
