from Box import Box
from IREntity import *


def lowerIndex(num):
    assert isinstance(num, int) 
    table = { '0': '₀', '1': '₁', '2': '₂', '3': '₃', '4': '₄', '5': '₅',
              '6': '₆', '7': '₇', '8': '₈', '9': '₉' }
    return ''.join(map(lambda c: table[c], str(num)))


# LazyExpr instances is should be used inside of a Box
# Immutable up to β equivalence
class LazyExpr:
    def __init__(self, args):
        self._args = args

    def isInWHNF(self):
        raise NotImplementedError()

    def toWHNF(self, it):
        raise NotImplementedError()

    def cloneWithArgs(self, args):
        raise NotImplementedError()

    def overwriteWith(self, newExpr):
        assert isinstance(newExpr, Box)
        assert isinstance(newExpr.getContent(), LazyExpr)
        self.parentBox.setContent(newExpr.getContent())

    def isConstructor(self):
        return False

    def str(self, visited=None):
        raise NotImplementedError()

    def _strImpl(self, argCount, args, visited):
        visited = set() if visited is None else visited
        if id(self) in visited: return "(...)"

        if len(args) >= argCount:
            lambdaArg = ""
        elif len(args) < argCount:
            argNames = "".join( "x%s" % lowerIndex(i)
                                for i in range(len(args), argCount) )
            lambdaArg = "λ%s." % argNames

        def toStr(arg):
            if isinstance(arg, Box):
                assert isinstance(arg.getContent(), LazyExpr)
                return arg.str(visited.union({id(self)}))
            return str(arg)

        expr = " ".join( toStr(arg) for arg in args )

        return "({}{})".format(lambdaArg, expr)


class LazyFunction(LazyExpr):
    def __init__(self, f, args):
        super().__init__(args)
        self._f = f

    def str(self, visited=None):
        return self._strImpl(1 + self._f.patternLen,
                             [self._f.name] + self._args,
                             visited)

    def cloneWithArgs(self, args):
        return Box(LazyFunction(self._f, self._args + args))

    # Returns dict ( varID → LazyApp )
    def _matchPattern(self, matchRule, it):
        assert len(matchRule) == self._f.patternLen
        bindings = {}

        def matchPattern(pattern, expr):
            if isinstance(pattern.bindingRef, Variable):
                assert len(pattern.args) == 0
                bindings[id(pattern.bindingRef)] = expr
                return True
            elif isinstance(pattern.bindingRef, Constructor):
                expr.toWHNF(it)
                if not expr.isConstructor():
                    raise RuntimeError("Pattern matching failed: argument is "
                                       + "not a constructor. (typing error?)")
                if id(pattern.bindingRef) != expr.getConstructor():
                    # pattern didn't match it's arguments
                    return False
                for patternArg, exprArg in zip(pattern.args, expr._args):
                    if not matchPattern(patternArg, exprArg):
                        return False
                return True
            else:
                assert False

        for pattern, arg in zip(matchRule, self._args):
            if not matchPattern(pattern, arg):
                return None
        return bindings

    @staticmethod
    def _substVars(expr, bindings):
        args = list( LazyFunction._substVars(arg, bindings)
                     for arg in expr.args )

        if isinstance(expr.funcRef, Variable):
            if not id(expr.funcRef) in bindings:
                raise RuntimeError('Unbinded variable')
            binding = bindings[id(expr.funcRef)]
            return Box(LazyApp(binding, args))

        elif isinstance(expr.funcRef, FunctionDef):
            return Box(LazyFunction(expr.funcRef, args))

        elif isinstance(expr.funcRef, Constructor):
            return Box(LazyConstructor(expr.funcRef, args))

        else:
            raise RuntimeError('Invalid function reference type')

    def isInWHNF(self):
        return self._f.patternLen > len(self._args)

    # WHNF = weak head normal form
    def toWHNF(self, it):
        boxed = self.parentBox # 'parentBox' field will be set to None later
        while not boxed.isInWHNF():
            boxed._toWHNFImpl(it)

    def _toWHNFImpl(self, it):
        if self.isInWHNF():
            return

        f = self._f

        for matchRule, expr in f.matchRules:
            bindings = self._matchPattern(matchRule, it)
            if bindings is None: continue
            next(it)
            bindedExpr = self._substVars(expr, bindings)
            # bindedExpr have been used anywhere yet (because _substVars always
            # returns brand new LazyExpr), so it's safe to modify it.
            # But let's not do that.
            fullExpr = bindedExpr.cloneWithArgs(self._args[f.patternLen:])
            self.overwriteWith(fullExpr)
            return
        else:
            raise RuntimeError('Pattern matching failed')


class LazyConstructor(LazyExpr):
    def __init__(self, c, args):
        super().__init__(args)
        self._c = c

        def countArgs(type):
            # TODO: Abstract constant
            if type.name != ['_→_']:
                return 0
            return 1 + countArgs(type.args[1])

        self._argCount = countArgs(c.type.typeExpr)

    def str(self, visited=None):
        return self._strImpl(self._argCount,
                             [self._c.name] + self._args,
                             visited)

    def cloneWithArgs(self, args):
        return Box(LazyConstructor(self._c, self._args + args))

    def isInWHNF(self):
        return True

    def toWHNF(self, it):
        return

    def isConstructor(self):
        return True

    def getConstructor(self):
        if len(self._args) < self._argCount:
            raise RuntimeError("Can't pattern match constructor: it doesn't "
                               + "have enough arguments (type error?)")
        elif len(self._args) > self._argCount:
            raise RuntimeError("Can't pattern match constructor: it has too "
                               + "many arguments (type error?)")
        return id(self._c)


class LazyApp(LazyExpr):
    def __init__(self, lazyExpr, args):
        super().__init__(args)
        self._expr = lazyExpr

    # Actually this method won't be called in this interpreter, but it's part
    # of interface.
    def cloneWithArgs(self, args):
        return Box(LazyApp, self._expr, self._args + args)


    def str(self, visited=None):
        return self._strImpl(1 + len(self._args),
                             [self._expr] + self._args,
                             visited)

    def isInWHNF(self):
        return False

    def toWHNF(self, it):
        # Evaluate subexpression first, to maximize sharing
        self._expr.toWHNF(it)
        newExpr = self._expr.cloneWithArgs(self._args)
        newExpr.toWHNF(it)
        self.overwriteWith(newExpr)

