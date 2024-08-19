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
        return result


class IllegalCharacterError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)


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

    def advance(self, current_char):
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
    def __init__(self, type_, value=None):
        self.type = type_
        self.value = value

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

        tokens.append(Token(TT_EOF))
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

        while self.current_char is not None and (self.current_char in DIGITS + '.'):
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str))
        else:
            return Token(TT_FLOAT, float(num_str))

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

###############
# PARSER
###############
# class Parser:
#     def __init__(self, tokens):
#         self.tokens = tokens
#         self.token_index = 1
#         self.advance()
#
#     def advance(self):
#         self.token_index += 1
#         if self.token_index < len(self.tokens):
#             self.current_token = self.tokens[self.token_index]
#         return self.current_token
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1  # Start before the first token
        self.advance()

    def advance(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        return self.current_token

#################################

    def parse(self):
        res = self.expression()
        return res

    def factor(self):
        tok = self.current_token

        if tok.type in (TT_INT,TT_FLOAT):
            self.advance()
            return NumberNode(tok)

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV , TT_MODULO))

    def expression(self):
        return self.bin_op(self.factor, (TT_PLUS, TT_MINUS))

#################################

    def bin_op(self, func, ops):
        left = func()

        while self.current_token.type in ops:
            op_tok = self.current_token
            self.advance()
            right = func()
            left = BinOpNode(left, op_tok, right)
        return left

###############
# RUN
###############


def run(fname, text):
    # Generate tokens
    lexer = Lexer(fname, text)
    tokens, errors = lexer.make_token()
    if errors: return None, errors

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()

    return ast, None
