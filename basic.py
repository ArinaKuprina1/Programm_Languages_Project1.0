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

    def make_token(self):
        tokens = []
        while self.current_char is not None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV))
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
                    return [], IllegalCharacterError(pos_start, self.pos, "'" + char + "'")
            elif self.current_char == '|':
                self.advance()
                if self.current_char == '|':
                    tokens.append(Token(TT_OR))
                    self.advance()
                else:
                    pos_start = self.pos.copy()
                    char = '|'
                    self.advance()
                    return [], IllegalCharacterError(pos_start, self.pos, "'" + char + "'")
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
                    return [], IllegalCharacterError(pos_start, self.pos, "'" + char + "'")
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
            elif self.current_char.isalpha() or self.current_char == '_':
                tokens.append(self.make_identifier())
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharacterError(pos_start, self.pos, "'" + char + "'")

        return tokens, None

    def make_identifier(self):
        id_str = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            id_str += self.current_char
            self.advance()

        if id_str == 'True' or id_str == 'TRUE' or id_str == 'true':
            return Token(TT_TRUE)
        elif id_str == 'False' or id_str == 'FALSE' or id_str == 'false':
            return Token(TT_FALSE)
        elif id_str == 'def':
            return Token(TT_DEF)
        elif id_str == 'lambda':
            return Token(TT_LAMBDA)
        else:
            # Unknown identifier
            pos_start = self.pos.copy()
            return Token('IDENTIFIER', id_str), IllegalCharacterError(pos_start, self.pos,
                                                                      f"Unknown identifier '{id_str}'")

    def make_number(self):
        num_str = ''
        dot_count = 0

        while self.current_char is not None and self.current_char in DIGITS + '.':
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

###############
# RUN
###############

def run(fname, text):
    lexer = Lexer(fname, text)
    tokens, errors = lexer.make_token()

    return tokens, errors
