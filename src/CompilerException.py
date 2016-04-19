class CompilerException(Exception):
    pass

# Error raises when name collision occures or name could not be resolved
class NameError(CompilerException):
    pass
