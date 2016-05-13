from collections import namedtuple
import re

from AHParserVisitor import AHParserVisitor
from CompilerException import CompilerException, NameError
from IREntityVisitor import EntityVisitor, EntityVisitorWithModuleStack
from AHParser import AHParser
from IREntity import Module, ImportedEntity, FunctionDef, TypeVariable, \
                     DataTypeDef, Constructor, Type, TypeExpr, Variable, Expr, \
                     PatternExpr, TypeVariable, Pragma, InternalFunction, \
                     Function, Entity
from AHBuiltinFunctionsFactory import getBuiltin
import AHBuiltinFunctions

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
            raise BadIR()

    def enterModule(self, module):
        try:
            super().enterModule(module)
            self.ensure(IR.isValidID(module.name))
            self.ensure(module.name in module.scope)
            self.ensure(all( isinstance(e, (FunctionDef, InternalFunction))
                             for e in module.functions ))
            self.ensure(all( isinstance(e, DataTypeDef) for e in module.datatypes ))
            self.ensure(all( isinstance(e, Module)     for e in module.submodules ))
            self.ensure(all( isinstance(e, ImportedEntity)
                             for e in module.importedEntities ))
        except:
            print('Bad: {}'.format(module))
            raise

    def enterImportedEntity(self, ie):
        try:
            # TODO: Should we also check that name is correctly resolved
            self.ensure(ie.importedName in self.currentModule.scope)
            self.ensure(IR.isValidQualifiedName(ie.originModuleName))
            self.ensure(IR.isValidID(ie.originName))
            self.ensure(IR.isValidID(ie.importedName))
            self.ensure(ie.entity is not None)
        except:
            print('Bad: {}'.format(ie))
            raise


    def enterFunctionDef(self, func):
        try:
            self.ensure(func.name in self.currentModule.scope)
            self.ensure(IR.isValidID(func.name))
            self.ensure(func.type is not None)
            self.ensure(func.patternLen is not None)
            for pattern, expr in func.matchRules:
                self.ensure(len(pattern) == func.patternLen)
                self.ensure(all( isinstance(patternExpr, PatternExpr)
                                 for patternExpr in pattern ))
                self.ensure(isinstance(expr, Expr))
        except:
            print('Bad: {}'.format(func))
            raise

    def enterInternalFunction(self, func):
        try:
            self.ensure(func.name in self.currentModule.scope)
            self.ensure(IR.isValidID(func.name))
            self.ensure(func.type is not None)
            self.ensure(len(func.depsNames) == len(func.deps))
            self.ensure(all( isinstance(e, Entity)
                             for e in func.deps.values() ))
        except:
            print('Bad: {}'.format(func))
            raise

    def enterDataTypeDef(self, datatype):
        try:
            self.ensure(datatype.name in self.currentModule.scope)
            self.ensure(IR.isValidID(datatype.name))
            self.ensure(all( IR.isValidID(arg) for arg in datatype.typeArgs ))
            self.ensure(all( isinstance(c, Constructor)
                             for c in datatype.constructors ))
            self.ensure(all( isinstance(a, TypeVariable)
                             for a in datatype.argRefs ))
            self.ensure(len(datatype.typeArgs) == len(datatype.argRefs))
        except:
            print('Bad: {}'.format(datatype))
            raise

    def enterConstructor(self, constructor):
        try:
            self.ensure(constructor.name in self.currentModule.scope)
            self.ensure(IR.isValidID(constructor.name))
            self.ensure(constructor.type is not None)
            self.ensure(constructor.datatype is not None)
        except:
            print('Bad: {}'.format(constructor))
            raise

    def enterType(self, type):
        try:
            self.ensure(all(IR.isValidID(arg) for arg in type.typeArgs))
            self.ensure(type.typeExpr is not None)
            self.ensure(len(type.typeArgs) == len(type.argRefs))
            self.ensure( all(isinstance(arg, TypeVariable)
                             for arg in type.argRefs ))
        except:
            print('Bad: {}'.format(type))
            raise

    def enterTypeExpr(self, typeExpr):
        try:
            self.ensure(IR.isValidQualifiedName(typeExpr.name))
            self.ensure(isinstance(typeExpr.typeRef, (DataTypeDef, TypeVariable)))
            self.ensure(all(isinstance(arg, TypeExpr)) for arg in typeExpr.args)
        except:
            print('Bad: {}'.format(typeExpr))
            raise

    def enterExpr(self, expr):
        try:
            self.ensure(IR.isValidQualifiedName(expr.name))
            self.ensure(isinstance(expr.funcRef, (Function, Constructor, Variable)))
            self.ensure(all(isinstance(arg, Expr) for arg in expr.args))
        except:
            print('Bad: {}'.format(expr))
            raise

    def enterPatternExpr(self, pe):
        try:
            self.ensure(IR.isValidQualifiedPatternName(pe.name))
            self.ensure(isinstance(pe.bindingRef, (Constructor, Variable)))
            if isinstance(pe.bindingRef, Variable):
                self.ensure(len(pe.args) == 0)
        except:
            print('Bad: {}'.format(pe))
            raise


class NameResolver(EntityVisitorWithModuleStack):
    def __init__(self):
        super().__init__()
        # locals: Name -> Entity
        self.variables = [{}]
        self.charDatatype = None

    @property
    def currentVariables(self):
        return self.variables[-1]

    def enterModule(self, module):
        super().enterModule(module)
        self.charDatatype = self._tryResolveName(['Char'])
        if self.charDatatype is None:
            raise CompilerException("Char datatype is not defined in module {}"
                                    .format(module.name))

    def exitModule(self, module):
        super().exitModule(module)
        self.charDatatype = None

    def enterMatchRule(self, pattern, expr):
        self.variables.append({}) # Add new scope

    def exitMatchRule(self, pattern, expr):
        self.variables.pop()

    def _addTypeVariables(self, vars, refs):
        newScope = {}
        for arg in vars:
            var = TypeVariable(arg)
            if arg == Variable.placeholder:
                raise RuntimeError("Placeholder is forbidden in type context")
            if arg in newScope:
                raise NameError("Name %s is used twice in type arguments" % arg)
            newScope[arg] = var
            refs.append(var)
        self.variables.append(newScope)

    def enterDataTypeDef(self, dt):
        self._addTypeVariables(dt.typeArgs, dt.argRefs)

    def exitDataTypeDef(self, dt):
        self.variables.pop()

    def enterType(self, type):
        self._addTypeVariables(type.typeArgs, type.argRefs)

    def exitType(self, type):
        self.variables.pop()

    def _tryResolveName(self, name):
        if len(name) == 1:
            maybeVariableName = name[0]
            for scope in reversed(self.variables):
                if maybeVariableName in scope:
                    return scope[maybeVariableName]
        return IR.walkPath(name, self.currentModule, 0)

    def enterInternalFunction(self, func):
        for name in func.depsNames:
            ref = self._tryResolveName(name)
            if ref is None:
                raise NameError("Can't resolve dependency {} of internal "
                                " function {}".format(name, func.name))
            func.deps[tuple(name)] = ref

    def enterTypeExpr(self, typeExpr):
        typeExpr.typeRef = self._tryResolveName(typeExpr.name)
        if typeExpr.typeRef is None:
            raise NameError("Can't resolve name %s in type expression"
                            % typeExpr.name)
        if not isinstance(typeExpr.typeRef, (TypeVariable, DataTypeDef)):
            raise NameError("Name %s doesn't refer to datatype or type variable"
                            % typeExpr.name)

    def enterPatternExpr(self, expr):
        expr.bindingRef = self._tryMakeChar(expr.name)
        if expr.bindingRef is not None:
            return

        entity = IR.walkPath(expr.name, self.currentModule, 0)
        if entity is not None: # Then this must be a constructor
            if expr.name == Variable.placeholder:
                raise RuntimeError("Placeholder binding found")
            if not isinstance(entity, Constructor):
                raise NameError("Name %s is not a constructor" % expr.name)
            expr.bindingRef = entity
        elif len(expr.name) == 1: # Then this must be a variable name
            if len(expr.args) > 0:
                raise NameError("Variable %s can't have arguments in pattern "
                                "expression" % variable.name)
            varName = expr.name[0]
            varRef = Variable(varName)
            expr.bindingRef = varRef
            if varName != Variable.placeholder:
                if varName in self.currentVariables:
                    raise NameError("Can't pattern match on same variable %s "
                                    "twice" % varName)
                self.currentVariables[varName] = varRef
        else:
            raise NameError("Can't find constructor %s" % expr.name)

    def _isCharLiteral(self, name):
        return len(name) == 1 and re.fullmatch("'.'", name[0]) is not None


    def _tryMakeChar(self, name):
        literal = name[0]

        if literal == "'\\n'":
            charValue = '\n'
        elif literal == "'\\s'":
            charValue = ' '
        elif re.fullmatch("'.'", literal) is not None:
            charValue = literal[1:-1]
        else:
            return None

        return AHBuiltinFunctions.CharConstructor(
                    self.charDatatype,
                    charValue)

    def enterExpr(self, expr):
        expr.funcRef =  self._tryMakeChar(expr.name)
        if expr.funcRef is not None:
            return

        expr.funcRef = self._tryResolveName(expr.name)
        if expr.funcRef is None:
            raise NameError("Can't resolve name %s in expression" % expr.name)
        if not isinstance(expr.funcRef, (Function, Constructor, Variable)):
            raise NameError("Name %s does not refer to function, constructor "
                            " or variable" % expr.name)
        if 'FORBID_IN_EXPR' in expr.funcRef.pragmaTags:
            raise NameError("Name %s can't be used in expressions because of "
                            "pragma" % expr.name)


class ParsedDataExtractor(AHParserVisitor):

    FuncDefTypePart = namedtuple('FuncDefTypePart', ('name, type'))
    FuncDefMatchRulePart = namedtuple('FuncDefMatchRulePart',
                                      ('name, pattern, expr'))

    class ImportList:
        def __init__(self, l):
            self.l = l
        def __iter__(self):
            return iter(self.l)

    class FuncDefBuilder:
        def __init__(self):
            self.typeParts = []
            self.matchRules = []
            self.builtins = {} # Name -> BuiltinName

        def addTypePart(self, typePart):
            self.typeParts.append(typePart)

        def addMatchRulePart(self, matchRule):
            self.matchRules.append(matchRule)

        def makeBuiltin(self, name, builtinName):
            if name in self.builtins:
                raise NameError("Can't make function {} builtin twice"
                                .format(name))
            self.builtins[name] = builtinName

        def getFunctions(self):
            funcs = {} # name: String -> FunctionDef
            
            for func in self.typeParts:
                if func.name in funcs:
                    raise NameError(
                        "Type of function {} has already been definied"
                        .format(func.name))
                if func.name in self.builtins:
                    try:
                        Builtin = getBuiltin(self.builtins[func.name])
                    except KeyError:
                        raise NameError('Builtin {} was not found'
                                        .format(self.builtins[func.name]))
                    funcDef = Builtin(name=func.name, type=func.type)
                else:
                    funcDef = FunctionDef(func.name)
                    funcDef.type = func.type
                funcs[func.name] = funcDef

            # Note: this preserves order of match rules
            for matchRule in self.matchRules:
                if not matchRule.name in funcs:
                    raise NameError(
                        "Function {} missing a type definition"
                        .format(func.name))
                if isinstance(funcs[matchRule.name], InternalFunction):
                    raise NameError("Internal function {} can't have pattern "
                                    "matching rules".format(matchRule.name))
                funcDef = funcs[matchRule.name]
                funcDef.matchRules.append((matchRule.pattern, matchRule.expr))

            for func in funcs.values():
                if isinstance(func, InternalFunction):
                    continue

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
                                          "arguments in function {}"
                                          .format(func.name))

            return funcs.values()

    def _addDefinitionsToModule(self, module, definitions):
        ImportList = ParsedDataExtractor.ImportList

        funcDefBuilder = self.FuncDefBuilder()

        def processPragma(pragma):
            if pragma.kind == 'INTERNAL':
                funcDefBuilder.makeBuiltin(pragma.entityName,
                                           pragma.builtinName)
            module.pragmas.append(pragma)

        matchType = [
            (Module,                    module.submodules.append),
            (DataTypeDef,               module.datatypes.append),
            (self.FuncDefTypePart,      funcDefBuilder.addTypePart),
            (self.FuncDefMatchRulePart, funcDefBuilder.addMatchRulePart),
            (ImportedEntity,            module.importedEntities.append),
            (ImportList,                module.importedEntities.extend),
            (Pragma,                    processPragma)
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

    def visitDefinitions(self, ctx:AHParser.DefinitionsContext):
        for child in ctx.getChildren():
            yield self.visit(child)

    def visitModule(self, ctx:AHParser.ModuleContext):
        name = self.visit(ctx.moduleHeader())
        module = Module(name)
        self._addDefinitionsToModule(module, self.visit(ctx.definitions()))
        return module

    def visitDataDef(self, ctx:AHParser.DataDefContext):
        dt = DataTypeDef(ctx.ID().getText())
        dt.typeArgs = self.visit(ctx.typeParams())
        for constructorCtx in ctx.constructorDef():
            c = self.visit(constructorCtx)
            c.datatype = dt
            dt.constructors.append(c)
        return dt

    def visitConstructorDef(self, ctx:AHParser.ConstructorDefContext):
        c = Constructor(ctx.ID().getText())
        c.type = self.visit(ctx.typeWithArgs())
        return c

    def visitFuncTypeDef(self, ctx:AHParser.FuncTypeDefContext):
        return ParsedDataExtractor.FuncDefTypePart(
                    name=ctx.ID().getText(),
                    type=self.visit(ctx.typeWithArgs()))

    def visitTypeWithArgs(self, ctx:AHParser.TypeWithArgsContext):
        t = Type()
        typeParams = ctx.typeParams()
        if typeParams is not None:
            t.typeArgs = self.visit(typeParams)
        t.typeExpr = self.visit(ctx.typeExpr())
        assert t.typeExpr is not None
        return t

    def visitFuncDef(self, ctx:AHParser.FuncDefContext):
        name = ctx.ID().getText()
        pattern = list(map(self.visit, ctx.pattern()))
        expr = self.visit(ctx.expr())
        return ParsedDataExtractor.FuncDefMatchRulePart(
                    name=name, pattern=pattern, expr=expr)

    # TODO: I don't remember, why it's commented
    # def visitImportStmt(self, ctx:AHParser.ImportStmtContext):
    #     return self.visitChildren(ctx)

    def visitPragmaStmt(self, ctx:AHParser.PragmaStmtContext):
        words = list(map(lambda t: t.getText(), ctx.ID()))
        return Pragma(words=words)

    def visitModuleImport(self, ctx:AHParser.ModuleImportContext):
        ie = ImportedEntity()
        ie.originModuleName = self.visit(ctx.qualifiedName())
        ie.originName = ie.originModuleName[-1]
        renaming = ctx.ID()
        assert len(renaming) == 0 or len(renaming) == 2
        if len(renaming) == 0:
            ie.importedName = ie.originName
        elif len(renaming) == 2:
            asWord, importedName = renaming
            if asWord != 'as':
                raise SyntaxError("Unexpected token {} when 'as' was expected "
                                  "in module import context"
                                  .format(asWord))
            ie.importedName = importedName

        return ie

    def visitEntityImport(self, ctx:AHParser.EntityImportContext):
        imports = []
        originModuleName = self.visit(ctx.qualifiedName())

        for importedNameCtx in ctx.importedName():
            ie = ImportedEntity()
            ie.originModuleName = originModuleName
            ie.originName, ie.importedName = self.visit(importedNameCtx)
            imports.append(ie)

        return ParsedDataExtractor.ImportList(imports)

    def visitPreserveNameImport(self, ctx:AHParser.PreserveNameImportContext):
        name = ctx.ID().getText()
        return (name, name)

    def visitRenamingImport(self, ctx:AHParser.RenamingImportContext):
        name, asWord, importedName = map(lambda t: t.getText(), ctx.ID())
        if asWord != 'as':
                raise SyntaxError("Unexpected token {} when 'as' was expected "
                                  "in entity import context"
                                  .format(asWord))
        return (name, importedName)

    def visitConcrTypeExpr(self, ctx:AHParser.ConcrTypeExprContext):
        return self.visit(ctx.concreteType())

    def visitArrowTypeExpr(self, ctx:AHParser.ArrowTypeExprContext):
        te = TypeExpr(['_→_'])
        te.args = list(map(self.visit, ctx.typeExpr()))
        return te

    def visitParenTypeExpr(self, ctx:AHParser.ParenTypeExprContext):
        return self.visit(ctx.typeExpr())

    def visitTypeParams(self, ctx:AHParser.TypeParamsContext):
        return list(map(lambda t: t.getText(), ctx.ID()))

    def visitConcreteType(self, ctx:AHParser.ConcreteTypeContext):
        te = TypeExpr(self.visit(ctx.qualifiedName()))
        te.args = list(map(self.visit, ctx.typeArg()))
        return te

    def visitNameTypeArg(self, ctx:AHParser.NameTypeArgContext):
        return TypeExpr(self.visit(ctx.qualifiedName()))

    def visitParenTypeArg(self, ctx:AHParser.ParenTypeArgContext):
        return self.visit(ctx.typeExpr())

    def visitNamePattern(self, ctx:AHParser.NamePatternContext):
        return PatternExpr(self.visit(ctx.qualifiedName()))

    def visitPlaceHolderPattern(self, ctx:AHParser.PlaceHolderPatternContext):
        return PatternExpr([ Variable.placeholder ])

    def visitParenPattern(self, ctx:AHParser.ParenPatternContext):
        return self.visit(ctx.patternExpr())

    def visitNormalPatternExpr(self, ctx:AHParser.NormalPatternExprContext):
        pe = PatternExpr(self.visit(ctx.qualifiedName()))
        pe.args = list(map(self.visit, ctx.pattern()))
        return pe

    def visitParenPatternExpr(self, ctx:AHParser.ParenPatternExprContext):
        pe = self.visit(ctx.patternExpr())
        pe.args.extend(map(self.visit, ctx.pattern()))
        return pe

    def visitExpr(self, ctx:AHParser.ExprContext):
        return self.visitChildren(ctx) # visit functionExpr

    def visitNormalFunctionExpr(self, ctx:AHParser.NormalFunctionExprContext):
        expr = Expr(self.visit(ctx.qualifiedName()))
        expr.args = list(map(self.visit, ctx.funcArg()))
        return expr

    def visitParenFunctionExpr(self, ctx:AHParser.ParenFunctionExprContext):
        expr = self.visit(ctx.functionExpr())
        expr.args.extend(map(self.visit, ctx.funcArg()))
        return expr

    def visitNameFuncArg(self, ctx:AHParser.NameFuncArgContext):
        return Expr(self.visit(ctx.qualifiedName()))

    def visitParenFuncArg(self, ctx:AHParser.ParenFuncArgContext):
        return self.visit(ctx.expr())

    def visitQualifiedName(self, ctx:AHParser.QualifiedNameContext):
        return list(map(lambda t: t.getText(), ctx.ID()))


# TODO: readonly is not ideal. Shouldn't work like that.
class IRBuilder:
    def __init__(self):
        self.readonly = False
        self.modules = {}  # name:String -> Module
        # Entities that are added to scope of every module in modules
        self.builtins = [] # List of Entity

    def checkReadonly(self):
        if self.readonly:
            raise RuntimeError("IRBuilder shouldn't be used after construction")

    def processPragmas(self, module):
        tempScope = { e.name: e for e in module.entities }
        tempScope[module.name] = module

        for pragma in module.pragmas:
            if hasattr(pragma, 'entityName'):
                if not pragma.entityName in tempScope:
                    raise NameError('Entity {} (referenced in pragma) not in '
                                    'module scope'.format(pragma.entityName))
            tempScope[pragma.entityName].pragmaTags.append(pragma.kind)

        for submodule in module.submodules:
            self.processPragmas(submodule)

    def addModule(self, module):
        self.checkReadonly()
        self.processPragmas(module)

        for e in module.entities:
            if 'BUILTIN' in e.pragmaTags: self.builtins.append(e)

        self.modules[module.name] = module

    # (TODO) a place for future improvements
    # Maybe add a package from wich module is from as argument (can be modeled
    # as submodules)
    def parseModule(self, parser):
        self.checkReadonly()
        extractor = ParsedDataExtractor()
        parseTree = parser.topLevelModule()
        module = extractor.visit(parseTree)
        self.addModule(module)

    def makeIR(self):
        if self.readonly:
            return IR(self.modules)
        for module in self.modules.values():
            module.populateScope(self.builtins)
        self.resolveImports()
        self.resolveNames()
        self.validate()
        self.typecheck()
        readonly = True
        return IR(self.modules)

    def getModule(self, qualifiedModuleName, fromModule):
        if not IR.isValidQualifiedName(qualifiedModuleName):
            raise BadIR("Invalid qualified name {}".format(qualifiedModuleName))

        module = IR.walkPath(qualifiedModuleName, fromModule, 0)
        if isinstance(module, Module):
            return module

        # Plan B: search from top module hierarchy
        topModuleName = qualifiedModuleName[0]
        if topModuleName in self.modules:
            module = IR.walkPath(qualifiedModuleName,
                                 self.modules[topModuleName], 1)
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

            for module, importedEntity in list(unresolvedImports):
                # Try to resolve a name
                originModule = self.getModule(importedEntity.originModuleName,
                                              module)
                if originModule is None:
                    continue # Module path potentially contains imported
                             # elements that are yet to be resolved

                if importedEntity.originName not in originModule.scope:
                    raise ImportError("Name {} isn't exported by module {} "
                                      "(required by {})"
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
        if not isinstance(other, IR):
            return False
        return Entity.compareAttr(self.modules, other.modules, set())

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
        # There might be loops in graph. We'll move entity reference two times
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
                                  "are detected at {}".format(path, i))

        if i == len(path):
            return entity

        if not isinstance(entity, Module):
            return None

        pathElement = path[i]

        if pathElement in entity.scope:
            nextEntity = entity.scope[pathElement]
            return IR.walkPath(path, nextEntity, i + 1)
        return None

