from lexer import Lexer
from parser import Parser

def interpretor(data: str):
    lex = Lexer(data)
    parse = Parser(lex)
    for p in parse.parse_program():
        out = p.eval()
    return out

if __name__ == "__main__":
    import sys
    from os.path import exists
    if len(sys.argv) < 2:
        print("file not given.")
        sys.exit(0)
    file_name = sys.argv[1]
    if not exists(file_name):
        print(f"{file_name} not found")
        sys.exit(0)
    with open(file_name) as f:
        data = f.read()
    interpretor(data) 
    