# TODO: it't not perfect to use global variables

# This module acts as a global factory for builtins
_REGISTERED_BUILTINS = {}

def getBuiltin(name):
    return _REGISTERED_BUILTINS[name]

# Decorator
def registerBuiltin(name):
    def decorate(obj):
        _REGISTERED_BUILTINS[name] = obj
        return obj
    return decorate
