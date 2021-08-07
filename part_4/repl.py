from lexer import Lexer
from parser import Parser
from tokens import *

def interpretor(data: str):
    lex = Lexer(data)
    parse = Parser(lex)
    
    for p in parse:
        if p == EOF:
            break
        p.eval()


def repl():
    i = 0
    while True:
        try:
            data: str = input(f"[{i}]: ")
            interpretor(data)
        except EOFError:           # handling ctrl + D
            print()
        except KeyboardInterrupt:  # handling ctrl + C 
            print("bye")
            break
        except Exception as msg:
            print(msg)
        finally:  
                i += 1   


if __name__ == "__main__":
    import sys
    from os.path import exists
    if len(sys.argv) < 2:
        repl()
    else:
        file_name = sys.argv[1]
        if not exists(file_name):
            print(f"{file_name} not found")
            sys.exit(0)
        with open(file_name) as f:
            data = f.read()
        interpretor(data) 
        