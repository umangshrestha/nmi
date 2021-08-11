class Stack(object):
    def __init__(self):
        self.stack = []

    def is_empty(self) -> bool:
        return len(self.stack) == 0

    def pop(self):
        if self.is_empty():
            raise Exception("stack is empty")
        return self.stack.pop()

    def push(self, value) -> None:
        self.stack.append(value)

    def print_stack(self) -> None:
        for item in self.stack:
            print(item)