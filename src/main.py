import sys
import os
from argparse import ArgumentParser

from AHLexer import AHLexer
from AHParser import AHParser
from antlr4 import InputStream, CommonTokenStream
from IR import IRBuilder
from Interpretator import LazyFunction, Box
from itertools import count


__location__ = os.path.realpath(
    os.path.join(os.getcwd(), os.path.dirname(__file__)))

PRELUDE_MODULE = os.path.join(__location__, 'Prelude.ahs')

# TODO: compiler to LLVM (possibly) (--compile flag?)
# TODO: tests for interpreter
# TODO: tests for code generation
# TODO: standard library
# TODO: pragmas and builtin module
# TODO: possibly string literals
# TODO: Type checker
# TODO: IO

def tracer(start, func):
    for i in count(0):
        print(i, ':', func())
        yield i

class Loader:
    def __init__(self):
        self.irbuilder = IRBuilder()
        self.ir = None

    def loadFile(self, filename):
        assert self.ir is None
        with open(filename) as file:
            self.irbuilder.parseModule(
                AHParser(
                CommonTokenStream(
                AHLexer(
                InputStream(file.read())))))

    def prepareIR(self):
        assert self.ir is None
        self.ir = self.irbuilder.makeIR()
        self.irbuilder = None

    def getInterpretator(self, moduleName, functionName):
        mainEntry = self.ir.modules[moduleName].scope[functionName]
        return Box(LazyFunction(mainEntry, []))


def main(argv):
    argParser = ArgumentParser(description='Compile AlmostHaskell program')
    argParser.add_argument('files', metavar='file',
                                    nargs='+',
                                    help='input sources')
    argParser.add_argument('--trace', action='store_true'
                                    , help='enables execution tracing')
    argParser.add_argument('--entry', nargs=2,
                                      metavar=('module', 'function'),
                                      default=['Main', 'main'],
                                      help='function to be executed (default --entry Main main)')
    args = argParser.parse_args(argv[1:])

    loader = Loader()
    loader.loadFile(PRELUDE_MODULE)
    for filename in args.files:
        loader.loadFile(filename)

    loader.prepareIR()
    lazyExpr = loader.getInterpretator(*args.entry)

    if args.trace:
        stepCounter = tracer(0, lambda: lazyExpr.str())
    else:
        stepCounter = count(0)
    lazyExpr.toWHNF(stepCounter)
    numSteps = next(stepCounter)

    print("\n===> main evaluated in", numSteps, "steps to:", lazyExpr.str())


if __name__ == "__main__":
    main(sys.argv)
