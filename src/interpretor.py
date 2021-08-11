LOAD_GLOBAL   = 1
LOAD_CONST    = 2
CALL_FUNCTION = 3

from stack import *

cmd = {
    "instruction" : [
        (LOAD_CONST, 1),
        (LOAD_GLOBAL, 0),
        (CALL_FUNCTION, 1),
    ],
    "co_consts" : (None, "Hello, World!"),
    "co_names" : ('print',)
}

class Interpretor(Stack):

    def __init__(self, codes: dict):
        Stack.__init__(self)
        self.instruction_pointer = 0
        self.codes = codes

    def LOAD_CONST(self, pos: int) -> None:
        value = self.codes["co_consts"][pos]
        self.push(value)

    def LOAD_GLOBAL(self, pos: int) -> None:
        value = self.codes["co_names"][pos]
        self.push(value)

    def PRINT(self, args: int):
        for arg in args:
            print(arg, end=' ')
        print(end='\n')
  
    def CALL_FUNCTION(self, pos):
        args = []
        func_name = self.pop()
        for _ in range(pos):
            value = self.pop()
            args.append(value)
        if  func_name == "print":
            self.PRINT(args)
        else:
            raise Exception("Function not found")

    def interpret(self):
        while self.instruction_pointer < len(self.codes):
            (opcode, oparg) = self.codes["instruction"][self.instruction_pointer]
           
            if opcode == LOAD_CONST:
                self.LOAD_CONST(oparg)
            elif opcode == LOAD_GLOBAL:
                self.LOAD_GLOBAL(oparg)
            elif opcode == CALL_FUNCTION: 
                self.CALL_FUNCTION(oparg)
            else:
                raise Exception(f"Invalid opcode {opcode}")
           
            self.instruction_pointer += 1

            



Interpretor(cmd).interpret()