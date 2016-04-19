import re
from enum import Enum
from PreLexer import PreLexer, Token, RecognitionException
from CompilerException import CompilerException


# TODO: move to utils?
def iterUntil(func, cond):
    res = func()
    while not cond(res):
        yield res
        res = func()


# Gives numbers to tokens, and generates a token specification for antlr parser
def _enumerateTokens(tokens):

    class EnumeratedTokens:
        @staticmethod
        def exportToAntlr():
            allTokens = EnumeratedTokens._allTokens
            return '\n'.join("{}={}".format(name, index)
                             for name, index in allTokens.items())

        @staticmethod
        def fromPreLexerTokenType(token):
            if token == Token.EOF:
                return Token.EOF
            tokenMap = EnumeratedTokens._fromPrelexerTokens
            if token in tokenMap:
                return tokenMap[token]
            raise RuntimeError("Invalid token value %d" % token)

        EOF = Token.EOF
        _allTokens = {}
        _fromPrelexerTokens = {}

    for i, token in enumerate(tokens, Token.MIN_USER_TOKEN_TYPE):
        for name in token:
            if not hasattr(EnumeratedTokens, name):
                setattr(EnumeratedTokens, name, i)
            EnumeratedTokens._allTokens[name] = i
            if hasattr(PreLexer, name):
                prelexerIndex = getattr(PreLexer, name)
                EnumeratedTokens._fromPrelexerTokens[prelexerIndex] = i

    return EnumeratedTokens


AHToken = _enumerateTokens([
    ('LParen', "'('"),
    ('RParen', "')'"),
    ('Dot', "'.'"),
    ('RArrow', "'->'", "'→'"),
    ('RDoubleArrow', "'=>'", "'⇒'"),
    ('Equal', "'='"),
    ('Underscore', "'_'"),
    ('Colon', "':'"),
    ('BeginBlock',),
    ('EndBlock',),
    ('Data', "'data'"),
    ('Where', "'where'"),
    ('Module', "'module'"),
    ('From', "'from'"),
    ('Import', "'import'"),
    ('NEWLINE',),
    ('ID',)])


def convertPreLexerTokenToAHToken(token):
    newToken = token.clone()
    newToken.type = AHToken.fromPreLexerTokenType(token.type)
    return newToken    


class LexerError(CompilerException):
    def __init__(self, msg, line, column, pos):
        super().__init__(msg)
        self.line = line
        self.column = column
        self.pos = pos

    def __str__(self):
        return "[{}-{}] {}".format(self.line, self.column, super().__str__())

# This lexer modifies output of PreLexer token stream in order to handle
# indentation. Most of the tokens are just past through.
# Several things to token stream are done:
# 1. Whenever token 'where' occures, a 'BeginBlock' token also emited and new
#    indented block (possibly empty) is started. Newline is not emited after
#    'where'. When indented block is ended token 'EndBlock' is emited.
# 2. If newline occures followed by increased indentation then it's treated as
#    line continuation and lines are connected and newline is not emitted
# 3. When several newlines in a row encountered all after the first one are
#    dropped. This includes the (2) rule.
# 4. Just before EOF it is ensured that newline token is present (otherwise it
#    is added)
#
# Implements TokenStream
class AHLexer:
    def __init__(self, input=None):
        # TODO: possibly clear error listeners and maybe modify error handling 
        #       strategy
        self._prelexer = PreLexer(input)
        self._indentations = [ ]
        self._state_handlers = {
            'FILE_START': self._state_file_start,
            'DEFAULT': self._state_default,
            'BEGIN_BLOCK': self._state_begin_block,
            'FIND_INDENTATION': self._state_find_indentation,
            'CLOSE_ALL_SCOPES': self._state_close_all_scopes,
            'CLOSE_SCOPE': self._state_close_scope,
            'EOF': self._state_eof
            }
        self._State = Enum('_State', list(self._state_handlers.keys()))
        self._state = self._State.FILE_START
        # Some states want to know what token is next, but postpone emitting it
        self._lookaheadToken = None

    # TokenSource interface
    def getCharPositionInLine(self):
        return self._prelexer.getCharPositionInLine()

    def getInputStream(self):
        return self._prelexer.getInputStream()

    def getLine(self):
        return self._prelexer.getLine()

    def getSourceName(self):
        return self._prelexer.getSourceName()

    def getTokenFactory(self):
        return self._prelexer.getTokenFactory()

    def nextToken(self):
        if self._state.name not in self._state_handlers:
            raise RuntimeError("Invalid state")
        return self._state_handlers[self._state.name]()

    def setTokenFactory(self, factory):
        return self._prelexer.setTokenFactory(factory)

    # TODO: potentially change implementation and add other erro listener methods
    def addErrorListener(self, listener):
        self._prelexer.addErrorListener(listener)

    def removeErrorListeners(self):
        self._prelexer.removeErrorListeners()

    # Private methods
    def _state_file_start(self):
        nextToken = self._skip_newlines()
        if nextToken.type == Token.EOF:
            self._push_indentation(0)
            return self._gotoState(self._State.EOF)
        self._push_indentation(nextToken.column)
        self._lookaheadToken = nextToken
        return self._gotoState(self._State.DEFAULT)

    def _state_default(self):
        if self._lookaheadToken is not None:
            nextToken = self._lookaheadToken
            self._lookaheadToken = None
        else:
            nextToken = self._prelexer.nextToken()

        if nextToken.type == PreLexer.NEWLINE:
            return self._maybe_newline(nextToken)
        elif nextToken.type == PreLexer.Where:
            self._state = self._State.BEGIN_BLOCK
            return convertPreLexerTokenToAHToken(nextToken)
        elif nextToken.type == Token.EOF:
            self._state = self._State.CLOSE_ALL_SCOPES
            return self._make_token(AHToken.NEWLINE, '\n')
        else:
            return convertPreLexerTokenToAHToken(nextToken)

    # Precondition: self._lookaheadToken is None
    def _state_begin_block(self):
        self._state = self._State.FIND_INDENTATION
        return self._make_token(AHToken.BeginBlock, "<Begin Block>")

    # Precondition: self._lookaheadToken is None
    def _state_find_indentation(self):
        nextToken = self._skip_newlines()
        currentIndentation = self._top_indentation()
        if nextToken.type == Token.EOF:
            # Empty block
            self._push_indentation(currentIndentation + 1)
            return self._gotoState(self._State.CLOSE_ALL_SCOPES)
        if nextToken.column > currentIndentation:
            self._push_indentation(nextToken.column)
            self._lookaheadToken = nextToken
            return self._gotoState(self._State.DEFAULT)
        else:
            # Empty block
            self._push_indentation(currentIndentation + 1)
            self._lookaheadToken = nextToken
            return self._gotoState(self._State.CLOSE_SCOPE)

    # Preconditions: * self._lookaheadToken is not None
    #                * lookaheadToken indentation is lower than top of
    #                  indentation stack
    def _state_close_scope(self):
        token = self._lookaheadToken
        assert token is not None
        assert token.column < self._top_indentation()
        if self._is_lowest_indentation_block():
            raise LexerError(
                "Indentation error. Indentation is lower than lowest block",
                token.line, token.column, token.start)
        self._pop_indentation()
        currentIndentation = self._top_indentation()
        if token.column < currentIndentation:
            pass # Continue closing scopes
        elif token.column == currentIndentation:
            # Close block and process lookahead token as usual
            self._state = self._State.DEFAULT
        else: # token.column > currentIndentation
            raise LexerError("Indentation error. Can't continue expression "
                             + "after closing block", token.line, token.column,
                             token.start)
        # TODO: creating tokens <Start Block> and <End Block> should be refactored
        return self._make_token(AHToken.EndBlock, "<End Block>")

    # Precondition: * self._lookaheadToken is None
    #               * last read token was EOF
    def _state_close_all_scopes(self):
        if self._is_lowest_indentation_block():
            return self._gotoState(self._State.EOF)
        self._pop_indentation()
        return self._make_token(AHToken.EndBlock, "<End Block>")

    # Precondition: * self._lookaheadToken is None
    #               * last read token was EOF
    #               * All indentation blocks are closed
    def _state_eof(self):
        return self._prelexer.emitEOF()

    def _maybe_newline(self, newlineToken):
        nextToken = self._skip_newlines()
        if nextToken.type == Token.EOF:
            self._state = self._State.CLOSE_ALL_SCOPES
            return convertPreLexerTokenToAHToken(newlineToken)

        currentIndentation = self._top_indentation()
        if nextToken.column == currentIndentation:
            # Alright, start new line
            self._lookaheadToken = nextToken
            self._state = self._State.DEFAULT
            return convertPreLexerTokenToAHToken(newlineToken)
        elif nextToken.column > currentIndentation:
            # Just continue previous line
            self._state = self._State.DEFAULT
            return convertPreLexerTokenToAHToken(nextToken)
        else:
            # It's new line and also closing scope
            self._lookaheadToken = nextToken
            self._state = self._State.CLOSE_SCOPE
            return convertPreLexerTokenToAHToken(newlineToken)

    def _is_lowest_indentation_block(self):
        return 1 == len(self._indentations)

    def _push_indentation(self, value):
        self._indentations.append(value)

    def _pop_indentation(self):
        return self._indentations.pop()

    def _top_indentation(self):
        return self._indentations[-1]

    def _skip_newlines(self):
        nextToken = self._prelexer.nextToken()
        while nextToken.type == PreLexer.NEWLINE:
            nextToken = self._prelexer.nextToken()
        return nextToken

    # TODO: this method name is not clear, it makes zero-width token
    def _make_token(self, type, text):
        pl = self._prelexer
        return pl._factory.create(
            pl._tokenFactorySourcePair,
            type,
            text,
            pl.DEFAULT_TOKEN_CHANNEL,
            pl._input.index,
            pl._input.index - 1,
            pl.line,
            pl.column
            )

    def _gotoState(self, state):
        self._state = state
        return self._state_handlers[state.name]()
