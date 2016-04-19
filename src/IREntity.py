from itertools import chain
from CompilerException import CompilerException, NameError

# TODO: abstract representation, fix equality, fix tests, finish visitor 

class Entity:
    entityKind = '(abstract entity)'
    _attrnames = None

    def __eq__(self, other):
        if self._attrnames is None:
            return None

        return (
                type(self) == type(other)
                and self._attrnames == other._attrnames
                and all( getattr(self, name) == getattr(other, name)
                         for name in self._attrnames )
               )

    def __hash__(self):
        return hash( tuple(getattr(self, name)
                     for name in self._attrnames))

    # Example output:
    # repr(entity)
    # >>> Entity(prop1="value",prop2=[4, 2])
    def __repr__(self):
        props = ",".join( "{}={}".format(name, getattr(self, name))
                          for name in self._attrnames )
        return "{}({})".format(self.entityKind, props)
                               

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
#      also. Variable names potentially could also be removed.
#
# Note: name resolution of imported entities depends on parent module (just add
#       parent module to as a field).
#
# Solutions:
# a. Make two type hierarchies: PreEntities and Entities. Will that work?
#    
# b. (nothing here)
class Module(Entity):
    entityKind = 'Module'
    _attrnames = ('name', 'functions', 'datatypes', 'submodules',
                  'importedEntities')

    def __init__(self, name=None):
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

    def __eq__(self, other):
        return (
                isinstance(other, Module)
                and self.name == other.name
                and set(self.functions)        == set(other.functions)
                and set(self.datatypes)        == set(other.datatypes)
                and set(self.submodules)       == set(other.submodules)
                and set(self.importedEntities) == set(other.importedEntities)
               )
    def __hash__ 

    def addEntityToScope(self, e):
        if e.name in self.scope:
            raise NameError("Name {}({}) was already defined as {}"
                            .format(e.name,
                                    e.entityKind,
                                    self.scope[e.name].entityKind))
        self.scope[e.name] = e

    def visit(self, visitor):
        visitor.enterModule(self)
        for e in chain(self.functions,
                       self.datatypes,
                       self.importedEntities,
                       self.submodules):
            e.visit(visitor)
        visitor.exitModule(self)

    def populateScope(self):
        self._addModuleEntitiesToScope()
        self._propagateScopeFromParent(self)

    def _addModuleEntitiesToScope(self):
        self.addEntityToScope(self)

        for e in chain(self.importedEntities,
                       self.submodules,
                       self.datatypes,
                       self.functions):
            self.addEntityToScope(e)

        for datatype in self.datatypes:
            for constructor in datatype.constructors:
                self.addEntityToScope(constructor)

        for module in self.submodules:
            module._addModuleEntitiesToScope()

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
        self.originModuleName = None # QualifiedName
        self.originName = None       # String
        self.importedName = None     # String
        self.entity = None           # Entity (needs to be resolved)
        self._attrnames = ('originModuleName', 'originName', 'importedName')

    @property
    def name(self):
        return self.importedName

    def visit(self, visitor):
        visitor.enterImportedEntity(self)
        visitor.exitImportedEntity(self)


class FunctionDef(Entity):
    entityKind = 'Function'
    _attrnames = ('name', 'type', 'patternLen', 'matchRules')

    def __init__(self, name=None):
        self.name = name       # String
        self.type = None       # Type
        self.patternLen = None # int
        self.matchRules = []   # List of (List of PatternExpr, Expr)
                               # Note: order of match rules is important
        self.submodule = None  # Submodule is per match rule, muppet!

    def visit(self, visitor):
        visitor.enterFunctionDef(self)
        if self.type is not None:
            self.type.visit(visitor)

        # TODO: Submodule handling is incorrect

        for pattern, expr in self.matchRules:
            visitor.enterMatchRule(pattern, expr)
            for patternExpr in pattern:
                patternExpr.visit(visitor)
            expr.visit(visitor)
            visitor.exitMatchRule(pattern, expr)
        visitor.exitFunctionDef(self)


# _→_ is builtin type constructor and should be imported in every module by
# default
class DataTypeDef(Entity):
    entityKind = 'Datatype'
    _attrnames = ('name', 'typeArgs', 'constructors')

    def __init__(self, name=None):
        self.name = name       # String
        self.typeArgs = []     # List of String
        self.constructors = [] # List of Constructor

    def visit(self, visitor):
        visitor.enterDataTypeDef(self)
        for constructor in self.constructors:
            constructor.visit(visitor)
        visitor.exitDataTypeDef(self)


class Constructor(Entity):
    entityKind = 'Constructor'
    _attrnames = ('name', 'type')

    def __init__(self, name=None, type=None, datatype=None):
        self.name = name         # String
        self.type = type         # Type
        self.datatype = datatype # DataTypeDef

    def visit(self, visitor):
        visitor.enterConstructor(self)
        if self.type is not None:
            self.type.visit(visitor)
        visitor.exitConstructor(self)


class Type(Entity):
    entityKind = 'Type'
    _attrnames = ('typeArgs', 'typeExpr')

    def __init__(self):
        self.typeArgs = []   # List of String
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

    def __init__(self, name):
        self.name = name


class TypeExpr(Entity):
    entityKind = 'TypeExpr'
    _attrnames = ('name', 'args')

    def __init__(self, name=None):
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
    _attrnames = ('name', 'args')

    def __init__(self, name=None):
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
    _attrnames = ('name', 'args')

    def __init__(self, name=None):
        self.name = name # QualifiedName. Either constructor, variable name or
                         # placeholder '_'
        self.maybeConstructorRef = None
        self.args = []   # List of PatternExpr

    def visit(self, visitor):
        visitor.enterPatternExpr(self)
        for arg in self.args:
            arg.visit(visitor)
        visitor.exitPatternExpr(self)
