# TODO:
# Use llvm lite to generate code

# If all is sky is not blue anymore, just interpret this b.s.

# LazyApp:

# LazyFunction(LazyApp)
#     arguments: List[LazyApp]

#     # Virtual means it's actually a pointer to function
#     virtual getConstructor()
#         # Pattern match on arguments (matching rule or it's runtime error)
#         # Bind variables
#         # Remove matched arguments from list
#         # Generate matching expression with variables substituted
#         # Append remaining arguments to the top of the expression
#         # Replace yourself inplace with top of expression
#         # Call getConstructor from top of the expression

# LazyConstructor(LazyApp)
#     arguments: List[LazeApp]
#     virtual getConstructor()
#         # return constructor id
#         # assert len(arguments) = constructor.lenArguments


# TODO: qualify import
from abc import ABCMeta

from llvmlite import ir
from itertools import repeat


class CodeGenerator:
    def __init__(self):
        pass

    # Adds function to compilation unit. All referenced entities are
    # automatically included in compilation.
    def addFunction(self, func):
        pass

    # Returns llvm ir containing code for all added functions
    def getCode(self):
        pass
