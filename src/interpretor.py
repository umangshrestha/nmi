LOAD_GLOBAL         = 1
LOAD_CONST          = 2
CALL_FUNCTION       = 3
# variable assignments
STORE_NAME          = 4
LOAD_NAME           = 5
DELETE_NAME         = 6
# arthemetic operations
BINARY_ADD          = 7
BINARY_SUBTRACT     = 8
BINARY_MULTIPLY     = 9
BINARY_TRUE_DIVIDE  = 10
BINARY_FLOOR_DIVIDE = 11
# logical operators
BINARY_OR           = 12
BINARY_AND          = 11



from stack import *


# a = 1 + 2
# print(a)
# del(a)
cmd = {
    "instruction" : [
        (LOAD_CONST, 2),    # stacking 1
        (LOAD_CONST, 1),    # stacking 2
        (BINARY_ADD, None), # 1+2 = 3
        (STORE_NAME, 1),    # a=3
        (LOAD_NAME, 1),     # 3
        (LOAD_GLOBAL, 0),   # stacking print
        (CALL_FUNCTION, 1), # print(3)
        (DELETE_NAME, 1),   # del(a)

    ],
    "co_consts" : (None, 1, 2),
    "co_names" : ("print", "a"),
}

#                  

class Interpretor(Stack):

    def __init__(self):
        Stack.__init__(self)
        self.instruction_pointer = 0
        self.codes = None
        self.environment = {}

    def LOAD_CONST(self, pos: int):
        value = self.codes["co_consts"][pos]
        self.push(value)

    def LOAD_GLOBAL(self, pos: int):
        value = self.codes["co_names"][pos]
        self.push(value)

    def LOAD_NAME(self, pos: int):
        name = self.codes["co_names"][pos]
        if name not in self.environment:
            raise NameError("name: {name} is not defined")
        value = self.environment[name]
        self.push(value)
    
    def DELETE_NAME(self, pos: int):
        name = self.codes["co_names"][pos]
        if name not in self.environment:
            raise NameError("name: {name} is not defined")
        self.environment.pop(name)
        

    def STORE_NAME(self, pos: int):
        value = self.stack.pop()
        name = self.codes["co_names"][pos]
        self.environment[name] = value

    def BINARY_ADD(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left + right)

    def BINARY_SUBTRACT(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left + right)

    def BINARY_MULTIPLY(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left * right)
  
    def BINARY_TRUE_DIVIDE(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left / right)
  
    def BINARY_FLOOR_DIVIDE(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left / right)
  
    def BINARY_TRUE_DIVIDE(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left / right)

    def BINARY_AND(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left & right)
  
    def BINARY_OR(self, _):
        left = self.pop()
        right = self.pop()
        self.push(left | right)
  
    def CALL_FUNCTION(self, pos):
        args = []
        func_name = self.pop()
        for _ in range(pos):
            value = self.pop()
            args.append(value)
        if  func_name == "print":
            print(*args)
        else:
            raise NameError(f"Function: {func_name} is not found")

    def __call__(self, cmd):
        self.codes = cmd
        while self.instruction_pointer < len(self.codes["instruction"]):
            (opcode, oparg) = self.codes["instruction"][self.instruction_pointer]
           
            
            function_map = {
                LOAD_CONST: self.LOAD_CONST,
                LOAD_GLOBAL: self.LOAD_GLOBAL,
                CALL_FUNCTION: self.CALL_FUNCTION,

                LOAD_NAME: self.LOAD_NAME,
                STORE_NAME: self.STORE_NAME,
                DELETE_NAME: self.DELETE_NAME,

                BINARY_ADD: self.BINARY_ADD,
                BINARY_SUBTRACT: self.BINARY_SUBTRACT,
                BINARY_TRUE_DIVIDE: self.BINARY_TRUE_DIVIDE,
                BINARY_FLOOR_DIVIDE: self.BINARY_FLOOR_DIVIDE,
                BINARY_MULTIPLY: self.BINARY_MULTIPLY,

                BINARY_AND: self.BINARY_AND,
                BINARY_OR : self.BINARY_OR,
                

            }
            if opcode not in function_map:
                raise Exception(f"Invalid opcode {opcode}")
            
            function_map[opcode](oparg)
          
            self.instruction_pointer += 1
            
interpret = Interpretor()
interpret(cmd)