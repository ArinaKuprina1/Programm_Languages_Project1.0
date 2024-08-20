from string_with_arrows import *

###############
# CONSTANTS
###############
DIGITS = '0123456789'


###############
# ERRORS
###############

class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}'
        result += f'File {self.pos_start.fname}, line{self.pos_start.line + 1}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftext, self.pos_start, self.pos_end)
        return result


class IllegalCharacterError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)

class InvalidSyntaxError(Error):
        def __init__(self, pos_start, pos_end, details=''):
                super().__init__(pos_start, pos_end, 'Invalid Syntax', details)


###############
# POSITION
###############
class Position:
    def __init__(self, index, line, column, fname, ftext):
        self.index = index
        self.line = line
        self.column = column
        self.fname = fname
        self.ftext = ftext

    def advance(self, current_char = None):
        self.index += 1
        self.column += 1

        if current_char == '\n':
            self.line += 1
            self.column += 0

        return self

    def copy(self):
        return Position(self.index, self.line, self.column, self.fname, self.ftext)


###############
# TOKENS
###############

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_TRUE = 'TRUE'
TT_FALSE = 'FALSE'
TT_MODULO = 'MODULO'
TT_AND = 'AND'
TT_OR = 'OR'
TT_NOT = 'NOT'
TT_EQ = 'EQ'
TT_NEQ = 'NEQ'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_DEF = 'DEF'
TT_LAMBDA = 'LAMBDA'
TT_LBRACE = '{'
TT_RBRACE = '}'
TT_LBRACKET = '['
TT_RBRACKET = ']'
TT_EOF = 'EOF'

# class Token:
#     def __init__(self, type_, value=None, pos_start=None, pos_end=None):
#         self.type = type_
#         self.value = value
#         if pos_start:
#             self.pos_start = pos_start.copy()
#             self.pos_end = pos_start.copy()
#             self.pos_end.advance()
#         if pos_end:
#             self.pos_end = pos_end
#
# class Token:
#     def __init__(self, type_, value=None, pos_start=None, pos_end=None):
#         self.type = type_
#         self.value = value
#         if pos_start:
#             self.pos_start = pos_start.copy()
#             self.pos_end = pos_start.copy()
#             self.pos_end.advance()
#         if pos_end:
#             self.pos_end = pos_end
#
#
#     def __repr__(self):
#         if self.value: return f'{self.type}: {self.value}'
#         return f'{self.type}'
class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        self.pos_start = pos_start.copy() if pos_start else None
        self.pos_end = pos_end.copy() if pos_end else pos_start.copy().advance() if pos_start else None

    def __repr__(self):
        if self.value:
            return f'{self.type}: {self.value}'
        return f'{self.type}'

###############
# LEXER
###############


class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, 0, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char in ' \t':
            self.advance()

    def skip_comment(self):
        while self.current_char is not None and self.current_char != '\n':
            self.advance()

    def handle_multiline_comment(self):
        self.advance()  # Skip the first '*'
        while self.current_char is not None:
            if self.current_char == '*':
                self.advance()
                if self.current_char == '/':
                    self.advance()
                    return [], None
            else:
                self.advance()
        # Error handling if no end of multiline comment found
        pos_start = self.pos.copy()
        return [], IllegalCharacterError(pos_start, self.pos, "Unterminated multiline comment")

    def make_token(self):
        tokens = []
        while self.current_char is not None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char == '#':
                self.advance()
                self.skip_comment()
            elif self.current_char == '/':
                self.advance()
                if self.current_char == '/':
                    self.advance()
                    self.skip_comment()
                elif self.current_char == '*':
                    self.advance()
                    result, error = self.handle_multiline_comment()
                    if error:
                        return [], error
                    tokens.extend(result)
                else:
                    tokens.append(Token(TT_DIV, pos_start=self.pos))
            elif self.current_char == '"':
                tokens.append(self.make_string())
            elif self.current_char.isalpha() or self.current_char == '_':
                token, error = self.make_identifier()
                if error:
                    return [], error
                tokens.append(token)
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '%':
                tokens.append(Token(TT_MODULO, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '{':
                tokens.append(Token(TT_LBRACE, pos_start=self.pos))
                self.advance()
            elif self.current_char == '}':
                tokens.append(Token(TT_RBRACE, pos_start=self.pos))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_LBRACKET, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_RBRACKET, pos_start=self.pos))
                self.advance()
            elif self.current_char == '&':
                self.advance()
                if self.current_char == '&':
                    tokens.append(Token(TT_AND, pos_start=self.pos))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '&'
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '|':
                self.advance()
                if self.current_char == '|':
                    tokens.append(Token(TT_OR, pos_start=self.pos))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '|'
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_NEQ, pos_start=self.pos))
                    self.advance()
                else:
                    tokens.append(Token(TT_NOT, pos_start=self.pos))
            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_EQ, pos_start=self.pos))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '='
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_GTE, pos_start=self.pos))
                    self.advance()
                else:
                    tokens.append(Token(TT_GT, pos_start=self.pos))
            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_LTE, pos_start=self.pos))
                    self.advance()
                else:
                    tokens.append(Token(TT_LT, pos_start=self.pos))
            elif self.current_char == '\\':
                pos_start = self.pos.copy()
                self.advance()
                if self.current_char in ['"', '\\']:
                    tokens.append(Token('STRING', self.current_char))  # Add escaped characters
                    self.advance()
                else:
                    return [], IllegalCharacterError(pos_start, self.pos,
                                                     f"Unexpected character '\\{self.current_char}'")
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None

    def make_identifier(self):
        id_str = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            id_str += self.current_char
            self.advance()

        if id_str.lower() == 'true':
            return Token(TT_TRUE,pos_start=self.pos), None
        elif id_str.lower() == 'false':
            return Token(TT_FALSE,pos_start=self.pos), None
        elif id_str == 'def':
            return Token(TT_DEF,pos_start=self.pos), None
        elif id_str == 'lambda':
            return Token(TT_LAMBDA,pos_start=self.pos), None
        else:
            pos_start = self.pos.copy()
            return Token('IDENTIFIER', id_str, pos_start, self.pos), IllegalCharacterError(pos_start, self.pos,
                                                                                           f"Unknown identifier '{id_str}'")
    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

    def make_string(self):
        pos_start = self.pos.copy()
        self.advance()  # Skip opening quote
        str_value = ''
        while self.current_char is not None and self.current_char != '"':
            if self.current_char == '\\':
                self.advance()
                if self.current_char == '"':
                    str_value += '"'
                elif self.current_char == '\\':
                    str_value += '\\'
                else:
                    str_value += '\\' + self.current_char
            else:
                str_value += self.current_char
            self.advance()
        self.advance()  # Skip closing quote
        return Token('STRING', str_value, pos_start, self.pos)

###############
# NODES
###############
class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node} {self.op_tok} {self.right_node})'


class BoolOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

    def __repr__(self):
        return f'({self.left_node} {self.op_tok} {self.right_node})'

class BoolNode:
    def __init__(self, tok):
        self.tok = tok

    def __repr__(self):
        return f'({self.tok})'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.node.pos_end

    def __repr__(self):
        return f'({self.op_tok} {self.node})'

class ComparisonNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

    def __repr__(self):
        return f'({self.left_node} {self.op_tok} {self.right_node})'

#######################################
# PARSE RESULT
#######################################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error: self.error = res.error
            return res.node

        return res

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self

###############
# PARSER
###############
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok

    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*' or '/'"
            ))
        return res


    ###################################


    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type == TT_NOT:
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))

        elif tok.type == TT_TRUE or tok.type == TT_FALSE:
            res.register(self.advance())
            return res.success(BoolNode(tok))

        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int or float or boolean"
        ))

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MODULO))


    def expr(self):

        res = ParseResult()
        left = res.register(self.bin_op(self.term, (TT_PLUS, TT_MINUS)))
        if res.error: return res

        while self.current_tok.type in (TT_AND, TT_OR):
                op_tok = self.current_tok
                res.register(self.advance())
                right = res.register(self.bin_op(self.term, (TT_AND, TT_OR, TT_NOT)))
                if res.error: return res
                left = BinOpNode(left, op_tok, right)

        # Add the comparison logic here
        while self.current_tok.type in (TT_EQ, TT_NEQ, TT_LT, TT_GT, TT_LTE, TT_GTE):
                op_tok = self.current_tok
                res.register(self.advance())
                right = res.register(self.comparison())
                if res.error: return res
                left = BinOpNode(left, op_tok, right)

        return res.success(left)

    def comparison(self):
        res = ParseResult()
        left = res.register(self.expr())
        if res.error: return res

        if isinstance(left, BoolNode):
            return res.failure(InvalidSyntaxError(
                left.tok.pos_start, left.tok.pos_end,
                "Cannot compare boolean with integer"
            ))

        while self.current_tok.type in (TT_EQ, TT_NEQ, TT_LT, TT_GT, TT_LTE, TT_GTE):
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(self.expr())
            if res.error: return res

            if isinstance(right, BoolNode):
                return res.failure(InvalidSyntaxError(
                    right.tok.pos_start, right.tok.pos_end,
                    "Cannot compare boolean with integer"
                ))

            left = ComparisonNode(left, op_tok, right)

        return res.success(left)

###################################

    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)
###############
# VALUES
###############
class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value)

    def multiply_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value)

    def divided_by(self, other):
        if isinstance(other, Number):
            return Number(self.value / other.value)

    def __repr__(self):
        return str(self.value)


###############
# INTERPRETER
###############
class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)

    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_NumberNode(self, node):
        return Number(node.tok.value).set_pos(node.pos_start, node.pos_end)

    def visit_UnaryOpNode(self, node):
        number = self.visit(node.node)
        if node.op_tok.type == TT_MINUS:
            number = number.multiply_by(Number(-1))
        return number.set_pos(node.pos_start, node.pos_end)

    def visit_BinOpNode(self, node):
        result = None
        left = self.visit(node.left_node)
        right = self.visit(node.right_node)
        if node.op_tok.value == TT_PLUS:
            result = left.added_to(right)
        elif node.op_tok.value == TT_MINUS:
            result = left.subbed_by(right)
        elif node.op_tok.value == TT_MUL:
            result = left.multiply_by(right)
        elif node.op_tok.value == TT_DIV:
            result = left.divided_by(right)

        return result.set_pos(node.pos_start, node.pos_end)

    # def visit_CompareNode(self, node):
    #     print("Found compare node!")
    #     self.visit(node.left_node)
    #     self.visit(node.right_node)

    def visit_BoolNode(self, node):
        print("Found bool node!")

    def visit_BoolOpNode(self, node):
        print("Found bool op node!")
        self.visit(node.left_node)
        self.visit(node.right_node)



###############
# RUN
###############


def run(fn, text):
    # Generate tokens
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_token()
    if error: return None, error

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    ##Run Program
    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    return result