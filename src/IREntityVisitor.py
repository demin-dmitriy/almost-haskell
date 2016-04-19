class EntityVisitor:
    def enterModule(self, module):
        pass

    def exitModule(self, module):
        pass

    def enterImportedEntity(self, importedEntity):
        pass

    def exitImportedEntity(self, importedEntity):
        pass

    def enterFunctionDef(self, function):
        pass

    def exitFunctionDef(self, function):
        pass

    def enterDataTypeDef(self, datatype):
        pass

    def exitDataTypeDef(self, datatype):
        pass

    def enterConstructor(self, constructor):
        pass

    def exitConstructor(self, constructor):
        pass

    def enterMatchRule(self, pattern, expr):
        pass

    def exitMatchRule(self, pattern, expr):
        pass

    def enterType(self, type):
        pass

    def exitType(self, type):
        pass

    def enterTypeExpr(self, typeExpr):
        pass

    def exitTypeExpr(self, typeExpr):
        pass

    def enterExpr(self, expr):
        pass

    def exitExpr(self, expr):
        pass

    def enterPatternExpr(self, patternExpr):
        pass

    def exitPatternExpr(self, patternExpr):
        pass


class EntityVisitorWithModuleStack(EntityVisitor):
    def __init__(self):
        self.moduleStack = []

    @property
    def currentModule(self):
        return self.moduleStack[-1]

    def enterModule(self, module):
        self.moduleStack.append(module)

    def exitModule(self, module):
        self.moduleStack.pop()
