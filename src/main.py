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

    # TODO: better to make separate class doing just translation
    # TODO: load builtin module
    irbuilder = IRBuilder()

    for filename in args.files:
        with open(filename) as file:
            irbuilder.parseModule(
                AHParser(
                CommonTokenStream(
                AHLexer(
                InputStream(file.read())))))

    ir = irbuilder.makeIR()

    moduleName, functionName = args.entry
    mainEntry = ir.modules[moduleName].scope[functionName]

    # TODO: should make factory methods
    lazyExpr = Box(LazyFunction(mainEntry, []))
    if args.trace:
        stepCounter = tracer(0, lambda: lazyExpr.str())
    else:
        stepCounter = count(0)
    lazyExpr.toWHNF(stepCounter)
    numSteps = next(stepCounter)

    print("main evaluated in", numSteps, "steps to:", lazyExpr.str())


if __name__ == "__main__":
    main(sys.argv)
