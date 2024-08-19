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


class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end

    def __repr__(self):
        if self.value: return f'{self.type}: {self.value}'
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
            elif self.current_char == '#':  # Start of a comment
                self.advance()
                self.skip_comment()
            elif self.current_char == '/':
                self.advance()
                if self.current_char == '/':  # Single-line comment
                    self.advance()
                    self.skip_comment()
                elif self.current_char == '*':  # Multi-line comment
                    self.advance()
                    result, error = self.handle_multiline_comment()
                    if error:
                        return [], error
                    tokens.extend(result)
                else:
                    tokens.append(Token(TT_DIV))
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
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL))
                self.advance()
            elif self.current_char == '%':
                tokens.append(Token(TT_MODULO))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN))
                self.advance()
            elif self.current_char == '{':
                tokens.append(Token(TT_LBRACE))
                self.advance()
            elif self.current_char == '}':
                tokens.append(Token(TT_RBRACE))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_LBRACKET))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_RBRACKET))
                self.advance()
            elif self.current_char == '&':
                self.advance()
                if self.current_char == '&':
                    tokens.append(Token(TT_AND))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '&'
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '|':
                self.advance()
                if self.current_char == '|':
                    tokens.append(Token(TT_OR))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '|'
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_NEQ))
                    self.advance()
                else:
                    tokens.append(Token(TT_NOT))
            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_EQ))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '='
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, f"Unexpected character '{char}'")
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_GTE))
                    self.advance()
                else:
                    tokens.append(Token(TT_GT))
            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    tokens.append(Token(TT_LTE))
                    self.advance()
                else:
                    tokens.append(Token(TT_LT))
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
            return Token(TT_TRUE), None
        elif id_str.lower() == 'false':
            return Token(TT_FALSE), None
        elif id_str == 'def':
            return Token(TT_DEF), None
        elif id_str == 'lambda':
            return Token(TT_LAMBDA), None
        else:
            pos_start = self.pos.copy()
            return Token('IDENTIFIER', id_str), IllegalCharacterError(pos_start, self.pos,
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
        return Token('STRING', str_value)

###############
# NODES
###############
class NumberNode:
    def __init__(self, tok):
        self.tok = tok

    def __repr__(self):
        return f'{self.tok}'

class BinOpNode:
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

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))

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
            "Expected int or float"
        ))

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MODULO))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

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

    return ast.node, ast.error