from typing import Any


__all__ = ["is_letter", "is_whitespace", "is_digit"]

def is_letter(value: Any) -> bool:
    "Checking if given value is letter"
    return  ("a" <= value <="z")   \
            | ("A" <= value <="Z") \
            | ('_' == value )


def is_whitespace(value: Any) -> bool:
    "Checking if the given value is whitespace"
    return  (value == "")     \
            | (value == "\t") \
            | (value == "\n") \
            | (value == "\r") 

def is_digit(value: Any) -> bool:
    "Checking if the given value is digit"
    return "0" <= value <= "9"