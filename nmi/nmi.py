from lexer import Lexer
from parser import Parser

def interpretor(file_name: str):
    with open(file_name) as f:
        data = f.read()
    
    lex = Lexer(data)
    parse = Parser(lex)
    for p in parse.parse_program():
        print(p)
        p.eval()

if __name__ == "__main__":
    import sys
    if len(sys.argv):
        file_name = sys.argv[1]
        interpretor(file_name)
    