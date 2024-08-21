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

class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def as_string(self):
        result  = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftext, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f'  File {pos.fname}, line {str(pos.line + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result


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
TT_DEFUN = 'DEFUN'
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
        elif id_str == 'defun':
            return Token(TT_DEFUN,pos_start=self.pos), None
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

        while self.current_char is not None and self.current_char in DIGITS + '.':
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

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

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
                "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
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

    def comp_expr(self):
        res = ParseResult()

        if self.current_tok.type == TT_NOT:
            op_tok = self.current_tok
            res.register(self.advance())

            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok, node))
        node = res.register(self.bin_op(self.arith_expr, (TT_EQ, TT_NEQ, TT_LT, TT_GT, TT_LTE, TT_GTE)))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected int or float or NOT")
            )

        return res.success(node)
    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def expr(self):

        res = ParseResult()
        left = res.register(self.bin_op(self.comp_expr, (TT_AND, TT_OR)))
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

#######################################
# RUNTIME RESULT
#######################################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error: self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self


#######################################
# VALUES
#######################################

class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Division by zero',
                    self.context
                )

            return Number(self.value / other.value).set_context(self.context), None

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Boolean(self.value == other.value).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Boolean(TT_TRUE if (self.value != other.value) else TT_FALSE).set_context(self.context), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Boolean(TT_TRUE if (self.value < other.value) else TT_FALSE).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Boolean(TT_TRUE if (self.value > other.value) else TT_FALSE).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(TT_TRUE if (self.value <= other.value)else TT_FALSE).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(TT_TRUE if (self.value >= other.value) else TT_FALSE).set_context(self.context), None


    def __repr__(self):
        return str(self.value)


class Boolean:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def anded_by(self, other):
        if isinstance(other, Boolean):
            return Boolean(TT_TRUE if(self.value == TT_TRUE and other.value == TT_TRUE) else TT_FALSE).set_context(self.context), None
             #  bool(self.value and other.value)).set_context(self.context), None

    def ored_by(self, other):
        if isinstance(other, Boolean):
            return Boolean(TT_TRUE if(self.value == TT_TRUE or other.value == TT_TRUE) else TT_FALSE).set_context(self.context), None

    def notted(self):
        return Boolean(TT_TRUE if self.value == TT_FALSE else TT_FALSE).set_context(self.context), None

    def __repr__(self):
        return 'true' if self.value == TT_TRUE else 'false'

#######################################
# CONTEXT
#######################################

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos


###############
# INTERPRETER
###############
class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    ###################################

    def visit_BoolNode(self, node, context):
        return RTResult().success(Boolean(node.tok.type).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.error: return res
        right = res.register(self.visit(node.right_node, context))
        if res.error: return res

        error = None
        result = None

        if isinstance(left, Number) :
            if node.op_tok.type == TT_PLUS:
                result, error = left.added_to(right)
            elif node.op_tok.type == TT_MINUS:
                result, error = left.subbed_by(right)
            elif node.op_tok.type == TT_MUL:
                result, error = left.multed_by(right)
            elif node.op_tok.type == TT_DIV:
                result, error = left.dived_by(right)
            elif node.op_tok.type == TT_EQ:
                result, error = left.get_comparison_eq(right)
            elif node.op_tok.type == TT_NEQ:
                result, error = left.get_comparison_ne(right)
            elif node.op_tok.type == TT_LT:
                result, error = left.get_comparison_lt(right)
            elif node.op_tok.type == TT_GT:
                result, error = left.get_comparison_gt(right)
            elif node.op_tok.type == TT_LTE:
                result, error = left.get_comparison_lte(right)
            elif node.op_tok.type == TT_GTE:
                result, error = left.get_comparison_gte(right)
        elif isinstance(left, Boolean) :
            if node.op_tok.type == TT_AND:
                result, error = left.anded_by(right)
            elif node.op_tok.type == TT_OR:
                result, error = left.ored_by(right)

        if error:
            return res.failure(error)
        if result is None:
            return res.failure(RTError(node.pos_start, node.pos_end,"Operation resulted is None", context))

        return res.success(result.set_pos(node.pos_start, node.pos_end))

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res

        error = None

        if node.op_tok.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.type == TT_NOT:
            if isinstance(number, Number):
                return res.failure(RTError(node.pos_start, node.pos_end, "Unable to perform NOT on number", context))
            number, error = number.notted()

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

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
    #return ast.node, ast.error

    # Run program
    interpreter = Interpreter()
    context = Context('<program>')
    result = interpreter.visit(ast.node, context)

    return result.value, result.error