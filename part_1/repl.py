from lexer import Lexer
from tokens import *

def repl():
    i = 0
    while True:
        try:
            data: str = input(f"[{i}]: ")
            for tok in Lexer(data):
                if tok.name != EOF :  
                    print(tok)
        except EOFError:           # handling ctrl + D
            print()
        except KeyboardInterrupt:  # handling ctrl + C 
            print("bye")
            break
        finally:  
                i += 1   
        
if __name__ == "__main__":
    repl()
  
        