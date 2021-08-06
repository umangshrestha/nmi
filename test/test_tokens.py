from tokens import NewEnum, precedence
import pytest

E = NewEnum('Enum', ["A", "B", "C"])

class TestNewEnum:

    @pytest.mark.parametrize("value1, value2", [
        (E.A, E.A),
        (E.B, E.B),
        (E.B, E.B),
    ])    
    def test_should_be_equal(self, value1, value2):
        assert value1 == value2
    
    @pytest.mark.parametrize("value1, value2", [

        (E.A, E.B),
        (E.A, E.C),
        (E.B, E.C),
    ])    
    def test_should_not_be_equal(self, value1, value2):
        assert value1 != value2


    @pytest.mark.parametrize("value1, value2", [
        (E.A, E.A),
        (E.B, E.B),
        (E.B, E.B),
        (E.A, E.B),
        (E.A, E.C),
        (E.B, E.C),
    ])    
    def test_should_be_less_or_equal(self, value1, value2):
        assert value1 <= value2


    @pytest.mark.parametrize("value1, value2", [
        (E.B, E.A),
        (E.C, E.B),
        (E.C, E.B),
    ])    
    def test_should_not_be_less_or_equal(self, value1, value2):
        assert not (value1 <= value2)