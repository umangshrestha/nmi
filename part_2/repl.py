from lexer import Lexer
from tokens import *

def read_file(file_name: str) -> None:
    with open(file_name) as f:
        data = f.read()
        for tok in Lexer(data):
            if tok.name == EOF:
                break
            print(tok, end="\t")
            
    
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
  
    read_file(file_name)