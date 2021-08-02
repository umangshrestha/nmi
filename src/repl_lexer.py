from lexer import Lexer
from tokens import Token

if __name__ == "__main__":
    i = 0
    while 1:
        try:
            data: str = input(f"[{i}]: ")
            lexer = Lexer(data)
            while tok := lexer.next_token():
                if tok.type == Token.EOF:  
                    break
                print(tok)
        except EOFError: 
            # handling ctrl + D
            print()
        except KeyboardInterrupt:
            # handling ctrl + C
            data = input(f"\nDO you want to exit? (Y/N)\n>")
            if data.lower() == "y":
                break
        except SyntaxError as msg:
            print(msg)
            # -2 is to remove \n\r in interpretor
            print(f"syntax error at line={i} pos={lexer.linepos-2}")
        finally: 
            i += 1   
        