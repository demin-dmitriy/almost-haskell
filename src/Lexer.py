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
        EOF = Token.EOF
        _allTokens = {}

    for i, token in enumerate(tokens, Token.MIN_USER_TOKEN_TYPE):
        for name in token:
            if not hasattr(EnumeratedTokens, name):
                setattr(EnumeratedTokens, name, i)
            EnumeratedTokens._allTokens[name] = i

    return EnumeratedTokens


AHToken = _enumerateTokens([
    ('LParen', "'('"),
    ('RParen', "')'"),
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
    ('NEWLINE',),
    ('ID',)])


def convertPreLexerTokenToAHToken(token):
    # TODO: probably, make calculation more direct
    if token.type == Token.EOF:
        return token # EOF token type is the same for all parsers

    for tokenTypeName in ('LParen', 'RParen', 'NEWLINE', 'Data', 'Where',
                          'Module', 'RArrow', 'RDoubleArrow', 'Equal',
                          'Underscore', 'Colon', 'ID'):
        if token.type == getattr(PreLexer, tokenTypeName):
            newToken = token.clone()
            newToken.type = getattr(AHToken, tokenTypeName)
            return newToken
    else:
        raise RuntimeError("Invalid token value %d" % token)


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


from unittest import TestCase
# Tests and helper methods that are common for PreLexer and AHLexer
class CommonLexerTestCase:

    class BasicErrorListener:
        def syntaxError(self, recogn, sym, line, column, msg, exc):
            raise LexerError("some error %s" % msg, line, column, None)

    def setLexer(self, LexerClass, TokenClass):
        self._LexerClass = LexerClass
        self._TokenClass = TokenClass

    def lex(self, str):
        from antlr4 import InputStream
        lexer = self._LexerClass(InputStream(str))
        lexer.removeErrorListeners()
        lexer.addErrorListener(CommonLexerTestCase.BasicErrorListener())
        tokens = []
        for token in iterUntil(lexer.nextToken,
                               lambda token: token.type == Token.EOF):
            tokens.append(token)
        return tokens

    def checkOutput(self, outTokens, correctTokens):
        self.assertEqual(len(outTokens), len(correctTokens))
        for outToken, correctToken in zip(outTokens, correctTokens):
            correctType, correctText = correctToken
            if correctType is not None:
                self.assertEqual(outToken.type, correctType)
            if correctText is not None:
                self.assertEqual(outToken.text, correctText)

    # For debug purposes
    def printOutput(self, outTokens):
        for token in outTokens:
            print(repr(token.text), '(' + str(token.type) + ')')

    def testID(self):
        test1 = self.lex(""" abc ->=∀x_:⊥]{-→2₂-- a"{d} b{--}e data1
            """)
        self.checkOutput(test1, [
            (self._TokenClass.ID, 'abc'),
            (self._TokenClass.ID, '->=∀x_:⊥]{-→2₂--'),
            (self._TokenClass.ID, 'a"{d}'),
            (self._TokenClass.ID, 'b{--}e'),
            (self._TokenClass.ID, 'data1'),
            (self._TokenClass.NEWLINE, '\n')
            ])

    def testComments(self):
        test1 = self.lex(
"""1 {- This
                    is
            comment  {- it 
                can be nested, if necessary
                     -}
            -}
2 -- Only two newlines should have been generated -} {-
one{--}token no-comment--here
            """)
        self.checkOutput(test1, [
            (self._TokenClass.ID, '1'),
            (self._TokenClass.NEWLINE, '\n'),
            (self._TokenClass.ID, '2'),
            (self._TokenClass.NEWLINE, '\n'),
            (self._TokenClass.ID, 'one{--}token'),
            (self._TokenClass.ID, 'no-comment--here'),
            (self._TokenClass.NEWLINE, '\n')
            ])

    # 'where' is special because it affects indentation rules
    def testCommonKeywords(self):
        test1 = self.lex("(()data) module -> → => ⇒ = _ : {= :--\n")
        self.checkOutput(test1, [
            (self._TokenClass.LParen, '('),
            (self._TokenClass.LParen, '('),
            (self._TokenClass.RParen, ')'),
            (self._TokenClass.Data, 'data'),
            (self._TokenClass.RParen, ')'),
            (self._TokenClass.Module, 'module'),
            (self._TokenClass.RArrow, '->'),
            (self._TokenClass.RArrow, '→'),
            (self._TokenClass.RDoubleArrow, '=>'),
            (self._TokenClass.RDoubleArrow, '⇒'),
            (self._TokenClass.Equal, '='),
            (self._TokenClass.Underscore, '_'),
            (self._TokenClass.Colon, ':'),
            (self._TokenClass.ID, '{='),
            (self._TokenClass.ID, ':--'),
            (self._TokenClass.NEWLINE, '\n')
            ])

    def testIllegalTabs(self):
        for test in ("\t", "\v", "\f"):
            self.assertRaises(LexerError, self.lex, test)

    def testInvalidComment(self):
        self.assertRaises(LexerError, self.lex, "abc {- ups!\n \n  ")
        self.assertRaises(LexerError, self.lex, "abc {- {- ouch!")
        self.assertRaises(LexerError, self.lex,
                    "a where {- -} -- ...\n {- {--} oh, not again ")

class PreLexerTest(TestCase, CommonLexerTestCase):
    def setUp(self):
        CommonLexerTestCase.setLexer(self, PreLexer, PreLexer)

    def testTokenWhere(self):
        self.checkOutput(self.lex("where"), [(PreLexer.Where, 'where')])

    def testNewlines(self):
        test1 = self.lex("""  a
 abc 


 {-

 -}
 def -- one
     -- two
     -- three
 ghi""")
        self.checkOutput(test1, [
            (PreLexer.ID, 'a'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.ID, 'abc'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.ID, 'def'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.ID, 'ghi')
            ])

        test2 = self.lex("a \r\n b \r c \n d")
        self.checkOutput(test2, [
            (PreLexer.ID, 'a'),
            (PreLexer.NEWLINE, '\r'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.ID, 'b'),
            (PreLexer.NEWLINE, '\r'),
            (PreLexer.ID, 'c'),
            (PreLexer.NEWLINE, '\n'),
            (PreLexer.ID, 'd')
            ])


class AHLexerTest(TestCase, CommonLexerTestCase):
    def setUp(self):
        CommonLexerTestCase.setLexer(self, AHLexer, AHToken)

    def testTokenWhere(self):
        self.checkOutput(self.lex("where"), [
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.EndBlock, None)
            ])
        test2 = self.lex("""
            data ℕ₀ where
                Zero : ℕ₀
                Succ : ℕ₀ → ℕ₀
        """)
        self.checkOutput(test2, [
            (AHToken.Data, 'data'),
            (AHToken.ID, 'ℕ₀'),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.ID, 'Zero'),
            (AHToken.Colon, ':'),
            (AHToken.ID, 'ℕ₀'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.ID, 'Succ'),
            (AHToken.Colon, ':'),
            (AHToken.ID, 'ℕ₀'),
            (AHToken.RArrow, '→'),
            (AHToken.ID, 'ℕ₀'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.EndBlock, None)
            ])

        test3 = self.lex("""
            module where
             a
             where
              b
                c
            d""")
        self.checkOutput(test3, [
            (AHToken.Module, 'module'),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.ID, 'a'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.ID, 'b'),
            (AHToken.ID, 'c'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.ID, 'd'),
            (AHToken.NEWLINE, None)
            ])

        test4 = self.lex("""
where where
        a
      b where
c
""")
        self.checkOutput(test4, [
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.ID, 'a'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.EndBlock, None),
            (AHToken.ID, 'b'),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.ID, 'c'),
            (AHToken.NEWLINE, '\n')
            ])

        test5 = self.lex("where where where")
        self.checkOutput(test5, [
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None)
            ])

        test6 = self.lex("where where where \n  \n\n ")
        self.checkOutput(test5, [
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.Where, 'where'),
            (AHToken.BeginBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None),
            (AHToken.EndBlock, None)
            ])

    def testNewlines(self):
        test1 = self.lex("a\n\n\n b")
        self.checkOutput(test1, [
            (AHToken.ID, 'a'),
            (AHToken.ID, 'b'),
            (AHToken.NEWLINE, None)])

        test2 = self.lex("a\n\n\nb\n c\n\n d")
        self.checkOutput(test2, [
            (AHToken.ID, 'a'),
            (AHToken.NEWLINE, '\n'),
            (AHToken.ID, 'b'),
            (AHToken.ID, 'c'),
            (AHToken.ID, 'd'),
            (AHToken.NEWLINE, None)
            ])

    def testBadIndentation(self):
        testBelowLowest1 = """
    firstToken
  badToken"""
        self.assertRaises(LexerError, self.lex, testBelowLowest1)

        testBelowLowest2 = """
    firstToken where
      abc where
        d
  badToken -- Oh no"""
        self.assertRaises(LexerError, self.lex, testBelowLowest2)

        testBadIndent1 = """
            a where
               blockContent
              badToken
        """
        self.assertRaises(LexerError, self.lex, testBadIndent1)
