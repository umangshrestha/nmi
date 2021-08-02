from lexer import Lexer
from parser import Parser


if __name__ == "__main__":
    i = 0
    while 1:
        try:
            data: str = input(f"[{i}]: ")
            lexer = Lexer(data)
            parser = Parser(lexer)
            parser.parse_program()
            program = parser.parse_program()
            while  len(program.statements) == 0:
                p = program.statements.pop()
                print(p.__class__.__name__,":",p.__repr__())
            if len(error :=parser.errors) != 0:
                for err in error:
                    print(err)
           
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
        