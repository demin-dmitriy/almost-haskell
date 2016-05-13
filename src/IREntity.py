from itertools import chain
from CompilerException import CompilerException, NameError

# TODO: abstract representation, fix equality, fix tests, finish visitor 

class Entity:
    entityKind = '(abstract entity)'
    _attrnames = None

    def __init__(self):
        # TODO: not sure what is best way to represent this
        self.pragmaTags = []

    @staticmethod
    def compareAttr(left, right, visitedSet):
        if type(left) is not type(right):
            return False

        if isinstance(left, (str, int)):
            return left == right

        if isinstance(left, (list, tuple)):
            return all(Entity.compareAttr(a, b, visitedSet)
                       for a, b in zip(left, right))


        if isinstance(left, dict):
            for key, left_value in left.items():
                if key not in right:
                    return False
                if not Entity.compareAttr(left_value, right[key], visitedSet):
                    return False
            return True

        if isinstance(left, Entity):
            return left.equals(right, visitedSet)

        if isinstance(left, set):
            # This will require hashes that work on recursive structures which
            # is problematic
            raise NotImplementedError("set comparision is not implemented")

        raise NotImplementedError("attributes have unknown type {}"
                                  .format(type(left)))

    # Equality that handles cyclic references
    def equals(self, other, visitedSet=None):
        assert self._attrnames is not None and other._attrnames is not None

        if visitedSet is None:
            visitedSet = set()

        objPair = id(self), id(other)
        if objPair in visitedSet: return True
        visitedSet.add(objPair)

        if type(self) is not type(other) or self._attrnames != other._attrnames:
            return False

        return all(self.compareAttr(
                        getattr(self, attr),
                        getattr(other,attr),
                        visitedSet)
                    for attr in self._attrnames)

    # Example output:
    # repr(entity)
    # >>> Entity(prop1="value",prop2=[4, 2])
    # We also take advantage of the fact that __repr__ by magically knows how to
    # handle recursive strucures.
    def __repr__(self):
        props = ",".join( "{}={}".format(name, getattr(self, name))
                          for name in self._attrnames )
        return "{}({})".format(self.entityKind, props)


class Pragma:
    pragmaKinds = [
        ('BUILTIN', 'entityName'),
        ('HIDDEN_MODULE', 'entityName'),
        ('ANY_TYPE', 'entityName'),
        ('INTERNAL', 'entityName', 'builtinName'),
        ('FORBID_IN_EXPR', 'entityName')
    ]

    def __init__(self, words):
        if len(words) < 1:
            raise CompilerException('Not a pragma')

        self.words = words
        self.kind = words[0]
        self.args = words[1:]

        for pragmaKind in self.pragmaKinds:
            kind = pragmaKind[0]

            if kind == self.kind:
                argNames = pragmaKind[1:]
                if len(argNames) != len(self.args):
                    raise CompilerException('Pragma %s has wrong number of '
                                            'arguments' % self)
                for arg, argName, in zip(self.args, argNames):
                    setattr(self, argName, arg)
                break
        else:
            raise CompilerException('Unknown pragma %s' % self)

    def __str__(self):
        return "Pragma({})".format(self.words)


# (TODO) State of the current things is far from ideal
# There several stages that Entity hierarchy is undergoing^
#   1.   Collect all information about functions datatypes, modules, imports
#      and store it module (Maybe we shouldn't do that. Because they exist
#      only to determine scope of the module and it's submodules). Maybe they
#      can be stored globally.
#   2.   Resolve all imports. Imported entities can be removed.
#   3.   Resolve all function, type, constructor and variable references. After
#      that all qualified names are no longer needed. At this point proper
#      equality testing requires matching ids because there are reference cycles
#      all over the place. All names except for variable names could be deleted
#      also. Variable names potentially could also be removed. Also modules are
#      not longer neccesary
#
# Note: name resolution of imported entities depends on parent module (just add
#       parent module to as a field).
#
# Solutions:
# a. Make two type hierarchies: PreEntities and Entities. Will that work?
#    
# b. (nothing here yet)
class Module(Entity):
    entityKind = 'Module'
    _attrnames = ('name', 'scope')

    def __init__(self, name=None):
        super().__init__()
        self.name = name        # String
        # All names (unqualified) in scope, including imported ones and module
        # itself. scope is used for name resolution
        # These names are exported by module
        # name:String -> Entity
        # Entity is one of: ImportedEntity, FunctionDef, DataTypeDef,
        #                   Constructor`, Module
        self.scope = {}            # (needs to be resolved)
        # These are entities that are actually stored in module itself
        self.functions = []        # List of FunctionDef
        self.datatypes = []        # List of DataTypeDef
        self.submodules = []       # List of Module
        self.importedEntities = [] # List of ImportedEntity
        self.pragmas = []          # List of Pragma

    # TODO: (stopped here). The correct solution is probably to rework class
    # hierarchy. But more quick and to the point one will be to just make and
    # compare frozensets in place and just close my eyes on that it's mutable
    # object that has a hash function (It won't be stored in hash maps as a key
    # and it won't ask for equality before it become 'immutable').
    # Other way is to compare scopes and handle recursive references somehow.
    # We'll need some kind of walker for that. Also recursive references make
    # it unconvinient to define correct answer. Maybe I can devise a label
    # system to accomplish that. Maybe then I don't need all this madness
    # with equality and hashes?!
#    def __hash__ 

    @property
    def entities(self):
        return chain(self.functions,
                     self.datatypes,
                     self.importedEntities,
                     self.submodules,
                     (c for d in self.datatypes for c in d.constructors))
    

    def addEntityToScope(self, e):
        if e.name in self.scope:
            if id(e) != id(self.scope[e.name]):
                raise NameError("Name {}({}) was already defined as {}"
                                .format(e.name,
                                        e.entityKind,
                                        self.scope[e.name].entityKind))
            return
        self.scope[e.name] = e

    def visit(self, visitor):
        visitor.enterModule(self)
        for e in chain(self.functions,
                       self.datatypes,
                       self.importedEntities,
                       self.submodules):
            e.visit(visitor)
        visitor.exitModule(self)

    def populateScope(self, builtins):
        self._addModuleEntitiesToScope(builtins)
        self._propagateScopeFromParent(self)

    def _addModuleEntitiesToScope(self, builtins):
        for e in builtins:
            self.addEntityToScope(e)

        self.addEntityToScope(self)

        for e in self.entities:
            self.addEntityToScope(e)

        for module in self.submodules:
            module._addModuleEntitiesToScope(builtins)

    # Called after _addModuleEntitiesToScope
    def _propagateScopeFromParent(self, parent):
        for name, entity in parent.scope.items():
            if name not in self.scope:
                self.scope[name] = entity
        for module in self.submodules:
            module._propagateScopeFromParent(self)


class ImportedEntity(Entity):
    entityKind = 'ImportedEntity'

    def __init__(self):
        super().__init__()
        self.originModuleName = None # QualifiedName
        self.originName = None       # String
        self.importedName = None     # String
        self.entity = None           # Entity (needs to be resolved)
        self._attrnames = ('originModuleName', 'originName', 'importedName',
                           'entity')

    @property
    def name(self):
        return self.importedName

    def visit(self, visitor):
        visitor.enterImportedEntity(self)
        visitor.exitImportedEntity(self)


class Function(Entity):
    def __init__(self, name=None, type=None):
        super().__init__()
        self.name = name # String
        self.type = type # Type

    # args should be fully resolved
    def withAppliedArgs(self, args):
        expr = Expr([self.name])
        expr.funcRef = self
        expr.args = args
        return expr

    def visit(self, visitor):
        if self.type is not None:
            self.type.visit(visitor)


class FunctionDef(Function):
    entityKind = 'Function'
    _attrnames = ('name', 'type', 'patternLen', 'matchRules')

    def __init__(self, name=None, type=None):
        super().__init__(name, type)
        self.patternLen = None # int
        self.matchRules = []   # List of (List of PatternExpr, Expr)
                               # Note: order of match rules is important
        self.submodule = None  # Submodule is per match rule, muppet!

    def visit(self, visitor):
        visitor.enterFunctionDef(self)
        super().visit(visitor)

        for pattern, expr in self.matchRules:
            visitor.enterMatchRule(pattern, expr)
            for patternExpr in pattern:
                patternExpr.visit(visitor)
            expr.visit(visitor)
            visitor.exitMatchRule(pattern, expr)

        visitor.exitFunctionDef(self)

class InternalFunction(Function):
    entityKind = 'InternalFunction'
    _attrnames = ('name', 'type', 'deps')

    def __init__(self, name=None, type=None):
        super().__init__(name, type)
        self.depsNames = [] # List of QualifiedName
        self.deps = {}      # QualifiedName (as tuple) → Entity

    def llvmImpl(self):
        raise NotImplementedError()

    def interpreterImpl(self, args): # Returns instance of LazyExpr
        raise NotImplementedError()

    def visit(self, visitor):
        visitor.enterInternalFunction(self)
        super().visit(visitor)
        visitor.exitInternalFunction(self)


class TypeVariable(Entity):
    entityKind = 'TypeVariable'
    _attrnames = ('name',)

    def __init__(self, name=None):
        super().__init__()
        self.name = name


# _→_ is builtin type constructor and should be imported in every module by
# default
class DataTypeDef(Entity):
    entityKind = 'Datatype'
    _attrnames = ('name', 'typeArgs', 'argRefs', 'constructors')

    def __init__(self, name=None):
        super().__init__()
        self.name = name       # String
        self.typeArgs = []     # List of String
        self.argRefs = []      # List of TypeVariable
        self.constructors = [] # List of Constructor

    # args should be fully resolved
    def withAppliedArgs(self, typeArgs):
        expr = TypeExpr([self.name])
        expr.typeRef = self
        expr.args = typeArgs
        return expr

    def visit(self, visitor):
        visitor.enterDataTypeDef(self)
        for constructor in self.constructors:
            constructor.visit(visitor)
        visitor.exitDataTypeDef(self)


class Constructor(Function):
    entityKind = 'Constructor'
    _attrnames = ('name', 'type', 'datatype')

    def __init__(self, name=None, type=None, datatype=None):
        super().__init__(name, type)
        self.datatype = datatype # DataTypeDef

    def getId(self):
        return id(self)

    def visit(self, visitor):
        visitor.enterConstructor(self)
        if self.type is not None:
            self.type.visit(visitor)
        visitor.exitConstructor(self)


class Type(Entity):
    entityKind = 'Type'
    _attrnames = ('typeArgs', 'argRefs', 'typeExpr')

    def __init__(self):
        super().__init__()
        self.typeArgs = []   # List of String
        self.argRefs = []    # List of TypeVariable
        self.typeExpr = None # TypeExpr

    def visit(self, visitor):
        visitor.enterType(self)
        if self.typeExpr is not None:
            self.typeExpr.visit(visitor)
        visitor.exitType(self)


class Variable(Entity):
    entityKind = 'Variable'
    _attrnames = ('name',)

    placeholder = '_' # In variable context doesn't bind to variable

    def __init__(self, name=None):
        super().__init__()
        self.name = name


class TypeExpr(Entity):
    entityKind = 'TypeExpr'
    _attrnames = ('name', 'args', 'typeRef')

    def __init__(self, name=None):
        super().__init__()
        self.name = name    # QualifiedName
        self.typeRef = None # DataTypeDef
        self.args = []      # List of TypeExpr

    def visit(self, visitor):
        visitor.enterTypeExpr(self)
        for arg in self.args:
            arg.visit(visitor)
        visitor.exitTypeExpr(self)


class Expr(Entity):
    entityKind = 'Expr'
    _attrnames = ('name', 'args', 'funcRef')

    # Is it even correct?
    # How to represent expression (f x) y ?
    # 
    # — We'll cheat a little and notice that if lambdas are not present than
    # (f x) y ≡ f x y for any valid expressions f, x, y
    def __init__(self, name=None):
        super().__init__()
        self.name = name    # QualifiedName
        self.funcRef = None # FunctionDef, Constructor or Variable
        self.args = []      # List of Expr

    def visit(self, visitor):
        visitor.enterExpr(self)
        for arg in self.args:
            arg.visit(visitor)
        visitor.exitExpr(self)


class PatternExpr(Entity):
    entityKind = 'PatternExpr'
    _attrnames = ('name', 'args', 'bindingRef')

    def __init__(self, name=None):
        super().__init__()
        self.name = name        # QualifiedName. Either constructor, variable
                                # name or placeholder '_'
        self.bindingRef = None  # Constructor or Variable
        self.args = []          # List of PatternExpr

    def visit(self, visitor):
        visitor.enterPatternExpr(self)
        for arg in self.args:
            arg.visit(visitor)
        visitor.exitPatternExpr(self)
