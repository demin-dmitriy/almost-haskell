from AHBuiltinFunctionsFactory import registerBuiltin
from IREntity import InternalFunction, Constructor, Type
from Interpretator import LazyExpr, LazyFunction
import sys
from Box import Box

def makeBuiltin(argcount, deps=[]):
    def make(betaReduce):
        class BuiltinTemplate(InternalFunction):
            class InterpreterImpl(LazyExpr):
                def __init__(self, f, args):
                    super().__init__(args)
                    self._intF = f

                def isInWHNF(self):
                    return len(self._args) < argcount

                def betaReduce(self, it):
                    return betaReduce(self, it)

                def cloneWithArgs(self, args):
                    return Box(BuiltinTemplate.InterpreterImpl(
                                self._intF, self._args + args))

                def str(self, visited=None):
                    return self._strImpl(1 + argcount,
                                         [self._intF.name] + self._args,
                                         visited)
                def _getDep(self, name):
                    return self._intF.deps[(name,)]

                def _makeExpr(self, struct):
                    funcStruct, argsStruct = struct
                    if isinstance(funcStruct, str):
                        func = self._getDep(funcStruct)
                    else:
                        func = funcStruct
                    args = list( self._makeExpr(s) for s in argsStruct )
                    return func.withAppliedArgs(args)

                def _makeLazyExpr(self, struct, bindings={}):
                    expr = self._makeExpr(struct)
                    return LazyFunction._substVars(expr, bindings)

            def __init__(self, name=None, type=None):
                super().__init__(name, type)
                self.depsNames = deps

            def interpreterImpl(self, args):
                return self.InterpreterImpl(self, args)

        return BuiltinTemplate
    return make


# Represents Char type efficiently
class CharConstructor(Constructor):
    def __init__(self, charDatatype, charValue):
        charType = Type()
        charType.typeExpr = charDatatype.withAppliedArgs([])
        super().__init__(
            name="'%s'" % str(charValue),
            type=charType,
            datatype=charDatatype)
        self.charValue = charValue

    def getId(self):
        return self.charValue


class IOConstructor(LazyExpr):
    argcount = 1
    name = 'io'

    def __init__(self, arg):
        super().__init__([arg])

    def str(self, visited=None):
        return self._strImpl(1 + self.argcount,
                             [self.name] + self._args, visited)

    def cloneWithArgs(self, args):
        return Box(IOConstructor(self._args[0]))

    def isInWHNF(self):
        return True

    def betaReduce(self, it):
        return

    def isConstructor(self):
        return True

    def getConstructor(self):
        raise RuntimeError('Pattern matching on IO constructor is not allowed')

    def getIOValue(self):
        return self._args[0]


# Bug: we depend on laziness to evaluate IO at the right order. However, strict
#      patterns on IO can break this. It will fail after evaluating io though.
@registerBuiltin('PUT_CHAR')
@makeBuiltin(argcount=1, deps=[['returnIO'], ['[]']])
def putCharBuiltin(self, it):
    if self.isInWHNF(): return

    arg = self._args[0]
    arg.toWHNF(it)
    if not arg.isConstructor():
        raise RuntimeError("Pattern matching failed: argument is "
                           "not a constructor. (typing error?)")

    next(it)

    charValue = arg.getConstructor()

    if not isinstance(charValue, str):
        raise RuntimeError("Type error: argument is not of type Char")

    sys.stdout.write(charValue)

    # returnIO unit
    self.overwriteWith(self._makeLazyExpr(
        ('returnIO', [('[]', [])])
        ))


@registerBuiltin('GET_CHAR')
@makeBuiltin(argcount=0, deps=[['returnIO'], ['Char']])
def putCharBuiltin(self, it):
    if self.isInWHNF(): return
    next(it)
    charValue = sys.stdin.read(1)
    # returnIO charValue
    charDatatype = self._getDep('Char')
    self.overwriteWith(self._makeLazyExpr(
        ('returnIO', [ (CharConstructor(charDatatype, charValue), []) ])
        ))


@registerBuiltin('RETURN_IO')
@makeBuiltin(argcount=1)
def returnIOBuiltin(self, it):
    if self.isInWHNF(): return
    arg = self._args[0]
    next(it)
    self.overwriteWith(Box(IOConstructor(arg)))


@registerBuiltin('BIND_IO')
@makeBuiltin(argcount=2)
def bindIOBuiltin(self, it):
    if self.isInWHNF(): return
    argLeft = self._args[0]
    argRight = self._args[1]

    argLeft.toWHNF(it)

    next(it)
    if not isinstance(argLeft.getContent(), IOConstructor):
        raise RuntimeError("Type error: argument is not of type IO")

    valueLeft = argLeft.getIOValue()
    self.overwriteWith(argRight.cloneWithArgs([valueLeft]))
