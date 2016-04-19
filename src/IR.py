# TODO: qualify imports
from collections import namedtuple

from AHParserVisitor import AHParserVisitor
from CompilerException import CompilerException, NameError
from IREntityVisitor import EntityVisitor, EntityVisitorWithModuleStack
from AHParser import AHParser
from IREntity import *


class SyntaxError(CompilerException):
    pass


class ImportError(CompilerException):
    pass


class BadIR(RuntimeError):
    pass


# Basicly all it does is check types of all entities in IR hierarchy.
class EntityValidator(EntityVisitorWithModuleStack):
    def __init__(self):
        super().__init__()

    def ensure(self, cond):
        if not cond:
            raise BadIR(msg)

    def enterModule(self, module):
        super().enterModule(module)
        self.ensure(IR.isValidID(module.name))
        self.ensure(module.name in module.scope)
        self.ensure(all(isinstance(e, FunctionDef) for e in module.functions))
        self.ensure(all(isinstance(e, DataTypeDef) for e in module.datatypes))
        self.ensure(all(isinstance(e, Module)      for e in module.submodules))
        self.ensure(all(isinstance(e, ImportedEntity)
                                             for e in module.importedEntities))

    def enterImportedEntity(self, ie):
        # TODO: Should we also check that name is correctly resolved
        self.ensure(ie.importedName in self.currentModule.scope)
        self.ensure(IR.isValidQualifiedName(ie.originModuleName))
        self.ensure(IR.isValidID(ie.originName))
        self.ensure(IR.isValidID(ie.importedName))
        self.ensure(ie.entity is not None)

    def enterFunctionDef(self, func):
        self.ensure(func.name in self.currentModule.scope)
        self.ensure(IR.isValidID(func.name))
        self.ensure(func.type is not None)
        self.ensure(func.patternLen is not None)
        for pattern, expr in func.matchRules:
            self.ensure(len(pattern) == func.patternLen)
            self.ensure(all(isinstance(patternExpr, PatternExpr)
                            for patternExpr in pattern))
            self.ensure(isinstance(expr, Expr))

    def enterDataTypeDef(self, datatype):
        self.ensure(datatype.name in self.currentModule.scope)
        self.ensure(IR.isValidID(datatype.name))
        self.ensure(all(IR.isValidID(arg) for arg in datatype.typeArgs))
        self.ensure(all(isinstance(c, Constructor)
                    for c in datatype.constructors))

    def enterConstructor(self, constructor):
        self.ensure(constructorname in self.currentModule.scope)
        self.ensure(IR.isValidID(constructor.name))
        self.ensure(constructor.type is not None)
        self.ensure(constructor.datatype is not None)

    def enterType(self, type):
        self.ensure(all(IR.isValidID(arg) for arg in type.typeArgs))
        self.ensure(type.typeExpr is not None)

    def enterTypeExpr(self, typeExpr):
        self.ensure(IR.isValidQualifiedName(typeExpr.name))
        self.ensure(typeExpr.typeRef is not None)
        self.ensure(isinstance(typeExpr.typeRef, DataTypeDef))
        self.ensure(all(isinstance(arg, TypeExpr)) for arg in typeExpr.args)

    def enterExpr(self, expr):
        self.ensure(IR.isValidQualifiedName(expr.name))
        self.ensure(expr.funcRef is not None)
        self.ensure(isinstance(expr.funcRef, (Function, Constructor)))
        self.ensure(all(isinstance(arg, Expr) for arg in expr.args))

    def enterPatternExpr(self, pe):
        self.ensure(IR.isValidQualifiedPatternName(pe.name))
        self.ensure(pe.maybeConstructorRef is None
                    or isinstance(pe.maybeConstructorRef, Constructor))


class NameResolver(EntityVisitorWithModuleStack):
    def __init__(self):
        super().__init__()
        # locals: Name -> Entity
        self.variables = [{}]

    @property
    def currentVariables(self):
        return self.variables[-1]

    def enterMatchRule(self, pattern, expr):
        self.variables.append({}) # Add new scope

    def exitMatchRule(self, pattern, expr):
        self.variables.pop()

    def enterType(self, type):
        newScope = {}
        for arg in type.typeArgs:
            if arg == Variable.placeholder:
                raise RuntimeError("Placeholder is forbidden in type context")
            if arg in newScope:
                raise NameError("Name %s is used twice in type arguments" % arg)
            newScope[arg] = Variable(arg)
        self.variables.append(newScope)

    def exitType(self, type):
        self.variables.pop()

    def _tryResolveName(self, name):
        if len(name) == 1:
            maybeVariableName = name[0]
            if maybeVariableName in self.currentVariables:
                return self.currentVariables[maybeVariableName]
        return IR.walkPath(name, self.currentModule, 0)

    def enterTypeExpr(self, typeExpr):
        typeExpr.typeRef = self._tryResolveName(typeExpr.name)
        if typeExpr.typeRef is None:
            raise NameError("Can't resolve name %s in type expression"
                                % typeExpr.name)
        if not isinstance(typeExpr.typeRef, (Variable, DataTypeDef)):
            raise NameError("Name %s doesn't refer to datatype or variable"
                            % typeExpr.name)

    def enterPatternExpr(self, expr):
        entity = IR.walkPath(expr.name, self.currentModule, 0)
        if entity is not None: # Then this must be a constructor
            if expr.name == Variable.placeholder:
                raise RuntimeError("Placeholder binding found")
            if not isinstance(entity, Constructor):
                raise NameError("Name %s is not a constructor" % expr.name)
            expr.maybeConstructorRef = entity
        elif len(expr.name) == 1: # Then this must be a variable name
            if len(expr.args) > 0:
                raise NameError("Variable %s can't have arguments in pattern "
                                + "expression" % variable.name)
            varName = expr.name[0]
            if varName != Variable.placeholder:
                if varName in self.currentVariables:
                    raise NameError("Can't pattern match on same variable %s "
                                    + "twice" % varName)
                self.currentVariables[varName] = Variable(varName)
        else:
            raise NameError("Can't find constructor %s" % expr.name)

    def enterExpr(self, expr):
        expr.funcRef = self._tryResolveName(expr.name)
        if expr.funcRef is None:
            raise NameError("Can't resolve name %s in expression" % expr.name)
        if not isinstance(expr.funcRef, (FunctionDef, Constructor, Variable)):
            raise NameError("Name %s does not refer to function, constructor "
                            + " or variable" % expr.name)


class ParsedDataExtractor(AHParserVisitor):

    FuncDefTypePart = namedtuple('FuncDefTypePart', ('name, type'))
    FuncDefMatchRulePart = namedtuple('FuncDefMatchRulePart',
                                      ('name, pattern, expr'))

    class FuncDefBuilder:
        def __init__(self):
            self.typeParts = []
            self.matchRules = []

        def addTypePart(self, typePart):
            self.typePart.append(typePart)

        def addMatchRulePart(self, matchRule):
            self.matchRule.append(matchRule)

        def getFunctions(self):
            funcs = {} # name: String -> FunctionDef
            
            for func in self.typeParts:
                if func.name in funcs:
                    raise NameError(
                        "Type of function {} has already been definied"
                        .format(func.name))
                funcDef = FunctionDef(func.name)
                funcDef.type = func.type
                funcs[func.name] = funcDef

            # Note: this preserves order of match rules
            for matchRule in self.matchRules:
                if not matchRule.name in funcs:
                    raise NameError(
                        "Function {} missing a type definition"
                        .format(func.name))
                funcDef = funcs[matchRule.name]
                funcDef.matchRules.append(matchRule.pattern, matchRule.expr)

            for func in funcs.values():
                if len(func.matchRules) == 0:
                    # TODO: I don't know if this should count as error or not
                    #       Maybe if I am not going to check that function is
                    #       total, then this shouldn't count as error.
                    func.patternLen = 0
                else:
                    patternLen = len(func.matchRules[0][0])
                    func.patternLen = patternLen
                    if not all(len(pattern) == patternLen 
                               for pattern, _ in func.matchRules):
                        raise SyntaxError("Different number of pattern matched "
                                          + "arguments in function {}"
                                          .format(func.name))

            return funcs.values()

    def _addDefinitionsToModule(self, module, definitions):
        funcDefBuilder = self.FuncDefBuilder()
        matchType = [
            (Module,                    module.submodules.append),
            (DataTypeDef,               module.datatypes.append),
            (self.FuncDefTypePart,      funcDefBuilder.addTypePart),
            (self.FuncDefMatchRulePart, funcDefBuilder.addMatchRulePart),
            (ImportedEntity,            module.importedEntities.append)
        ]

        for definition in definitions:
            for T, store in matchType:
                if isinstance(definition, T):
                    store(definition)
                    break
            else:
                raise RuntimeError("Invalid definition type {}"
                                   .format(definition))

        module.functions.extend(funcDefBuilder.getFunctions())

    def visitModuleHeader(self, ctx:AHParser.ModuleHeaderContext):
        return ctx.ID().getText()

    def visitTopLevelModule(self, ctx:AHParser.TopLevelModuleContext):
        return self.visitModule(ctx)

    # Visit a parse tree produced by AHParser#definitions.
    def visitDefinitions(self, ctx:AHParser.DefinitionsContext):
        for child in ctx.getChildren():
            yield self.visit(child)

    def visitModule(self, ctx:AHParser.ModuleContext):
        name = self.visit(ctx.moduleHeader())
        module = Module(name)
        self._addDefinitionsToModule(module, self.visit(ctx.definitions()))
        return module

    # Visit a parse tree produced by AHParser#dataDef.
    def visitDataDef(self, ctx:AHParser.DataDefContext):
        return self.visitChildren(ctx)

    # Visit a parse tree produced by AHParser#constructorDef.
    def visitConstructorDef(self, ctx:AHParser.ConstructorDefContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#funcTypeDef.
    def visitFuncTypeDef(self, ctx:AHParser.FuncTypeDefContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#funcDef.
    def visitFuncDef(self, ctx:AHParser.FuncDefContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#importStmt.
    def visitImportStmt(self, ctx:AHParser.ImportStmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#moduleImport.
    def visitModuleImport(self, ctx:AHParser.ModuleImportContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#entityImport.
    def visitEntityImport(self, ctx:AHParser.EntityImportContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#importedName.
    def visitImportedName(self, ctx:AHParser.ImportedNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#typeExpr.
    def visitTypeExpr(self, ctx:AHParser.TypeExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#typeParams.
    def visitTypeParams(self, ctx:AHParser.TypeParamsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#concreteType.
    def visitConcreteType(self, ctx:AHParser.ConcreteTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#typeArg.
    def visitTypeArg(self, ctx:AHParser.TypeArgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#pattern.
    def visitPattern(self, ctx:AHParser.PatternContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#patternArg.
    def visitPatternArg(self, ctx:AHParser.PatternArgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#expr.
    def visitExpr(self, ctx:AHParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#functionExpr.
    def visitFunctionExpr(self, ctx:AHParser.FunctionExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#funcArg.
    def visitFuncArg(self, ctx:AHParser.FuncArgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AHParser#qualifiedName.
    def visitQualifiedName(self, ctx:AHParser.QualifiedNameContext):
        return self.visitChildren(ctx)


class IRBuilder:
    def __init__(self):
        self.readonly = False
        # TODO: maybe add module named "{-builtins-}?
        self.modules = {} # name:String -> Module

    def checkReadonly(self):
        if self.readonly:
            raise RuntimeError("IRBuilder shouldn't be used after construction")

    # (TODO) a place for future improvements
    # Maybe add a package from wich module is from as argument (can be modeled
    # as submodules)
    def addModule(self, parser):
        self.checkReadonly()
        extractor = ParsedDataExtractor()
        parseTree = parser.topLevelModule()
        module = extractor.visit(parseTree)
        self.modules[module.name] = module

    def makeIR(self):
        if self.readonly:
            return IR(self.modules)
        for module in self.modules.values():
            module.populateScope()
        self.resolveImports()
        self.resolveNames()
        self.validate()
        self.typecheck()
        readonly = True
        return IR(self.modules)

    def getModule(self, qualifiedModuleName, fromModule):
        if not IR.isValidQualifiedName(qualifiedModuleName):
            raise BadIR("Invalid qualified name {}".format(qualifiedModuleName))

        module = IR.walk(qualifiedModuleName, fromModule, 0)
        if isinstance(module, Module):
            return module

        # Plan B: search from top module hierarchy
        topModuleName = qualifiedModuleName[0]
        if topModuleName in self.modules:
            module = IR.walkPath(qualifiedModuleName,
                                 self.modules[moduleName], 1)
            if isinstance(module, Module):
                return module
        return None

    def getAllUnresolvedImports(self):
        unresolvedImports = set()

        def populateUnresolvedImports(module):
            for ie in module.importedEntities:
                if ie.entity is None:
                    unresolvedImports.add((module, ie))
            for submodule in module.submodules:
                populateUnresolvedImports(submodule)

        for module in self.modules.values():
            populateUnresolvedImports(module)

        return unresolvedImports

    # Walks all modules and resolves ImporedEntities
    def resolveImports(self):
        self.checkReadonly()

        # (TODO) this a place for future improvements
        # Inefficient but good enough as long as maximum depth of
        # ImportedEntities is not to deep

        unresolvedImports = self.getAllUnresolvedImports()

        while len(unresolvedImports) != 0:
            anImportWasResolved = False

            for module, importedEntity in unresolvedImports:
                # Try to resolve a name
                originModule = self.getModule(importedEntity.originModuleName,
                                              module)
                if originModule is None:
                    continue # Module path potentially contains imported
                             # elements that are yet to be resolved

                if importedEntity.originName not in originModule.scope:
                    raise ImportError("Name {} isn't exported by module {} "
                                      + "(required by {})"
                                      .format(importedEntity.name,
                                              importedEntity.originModuleName,
                                              module.name))

                entity = originModule.scope[importedEntity.originName]
                importedEntity.entity = entity
                anImportWasResolved = True
                unresolvedImports.remove((module, importedEntity))

            if not anImportWasResolved: # We stuck
                break
        else:
            return # success

        # (TODO): for future imporovements: We can detect here which modules are
        #         not resolved
        unresolvedStr = ",".join(
                        "{}(in module {})"
                            .format(importedEntity.originName, module.name)
                        for module, importedEntity in unresolvedImports)
        raise ImportError("Couldn't resolve imports: %s" % unresolvedStr)

    # Resolves all references to other entities in expressions (pattern, type,
    #   function)
    def resolveNames(self):
        nr = NameResolver()
        for module in self.modules.values():
            module.visit(nr)

    # Check internal representation consistency
    # All names should be resolved.
    # raises BadIR if inconsistency is detected
    def validate(self):
        ev = EntityValidator()
        for module in self.modules.values():
            module.visit(ev)

    # Currently only simplest typechecking is implemented. No second order
    # polymorphism, unfortunately
    def typecheck(self):
        pass # TODO


class IR:
    def __init__(self, modules):
        self.modules = modules # name:String -> Module

    def __eq__(self, other):
        return isinstance(other, IR) and self.modules == other.modules

    def __repr__(self):
        return "IR(modules={})".format(repr(self.modules))

    @staticmethod
    def isValidID(id):
        return isinstance(id, str) and id != Variable.placeholder

    @staticmethod
    def isValidQualifiedName(name):
        return isinstance(name, list) \
            and len(name) > 0 \
            and all(IR.isValidID(part) for part in name)

    @staticmethod
    def isValidQualifiedPatternName(name):
        return isinstance(name, list) \
            and ((len(name) == 1 and isinstance(name[0], str))
                or
                (len(name) > 1 and all(IR.isValidID(part) for part in name)))

    @staticmethod
    def makeQualifiedName(*parts):
        return list(parts)

    # Returns None if entity could not be found. This includes the case
    # when not all of the ImportedEntity are resolved
    @staticmethod
    def walkPath(path, entity, i):
        # ImportedEntities are transperent
        # There might loops in graph. We'll move entity reference two times
        # faster than slowPointerBehind and if they ever become equal then it
        # means there is a loop.
        slowPointerBehind = entity
        while isinstance(entity, ImportedEntity):
            slowPointerBehind = slowPointerBehind.entity
            entity = entity.entity
            if not isinstance(entity, ImportedEntity):
                break
            entity = entity.entity
            if slowPointerBehind == entity:
                raise ImportError("Path {} can't be resolved: cyclic imports "
                                  + "are detected at {}".format(path, i))

        if i == len(path):
            return entity

        if not isinstance(entity, Module):
            return None

        pathElement = path[i]

        if pathElement in entity.scope:
            nextEntity = entity.scope[pathElement]
            return IR.walkPath(path, nextEntity, i + 1)
        return None

