from unittest import TestCase
from Lexer import *

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

    def testQualifiedName(self):
        test1 = self.lex("a.b.c\n")
        self.checkOutput(test1, [
            (self._TokenClass.ID, 'a'),
            (self._TokenClass.Dot, '.'),
            (self._TokenClass.ID, 'b'),
            (self._TokenClass.Dot, '.'),
            (self._TokenClass.ID, 'c'),
            (self._TokenClass.NEWLINE, '\n')
            ])

        test2 = self.lex("cba  . abc. de .f  \n")
        self.checkOutput(test2, [
            (self._TokenClass.ID, 'cba'),
            (self._TokenClass.Dot, '.'),
            (self._TokenClass.ID, 'abc'),
            (self._TokenClass.Dot, '.'),
            (self._TokenClass.ID, 'de'),
            (self._TokenClass.Dot, '.'),
            (self._TokenClass.ID, 'f'),
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
        test1 = self.lex("(()data) module from import -> → => ⇒ = _ : {= :--\n")
        self.checkOutput(test1, [
            (self._TokenClass.LParen, '('),
            (self._TokenClass.LParen, '('),
            (self._TokenClass.RParen, ')'),
            (self._TokenClass.Data, 'data'),
            (self._TokenClass.RParen, ')'),
            (self._TokenClass.Module, 'module'),
            (self._TokenClass.From, 'from'),
            (self._TokenClass.Import, 'import'),
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
