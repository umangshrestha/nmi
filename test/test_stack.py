from stack import Stack

class TestStack:
    def setup_class(self):
        self.stack = Stack()

    def test_push(self):
        self.stack.push(10)
        assert self.stack.is_empty() == False

    def test_pop(self):
        assert self.stack.pop() == 10

    def test_is_empty(self):
        assert self.stack.is_empty() == True