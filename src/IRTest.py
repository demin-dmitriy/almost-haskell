from unittest import TestCase
from IR import *


class IRTest(TestCase):
    def parse(self, s):
        from Lexer import AHLexer
        from antlr4 import InputStream, CommonTokenStream
        from AHParser import AHParser
        parser = AHParser(CommonTokenStream(AHLexer(InputStream(s))))
        irbuilder = IRBuilder()
        irbuilder.addModule(parser)
        return irbuilder.makeIR()

    # Helper method for defining nested structures
    @staticmethod
    def make(struct):
        if isinstance(struct, (str, int)):
            return struct
        if isinstance(struct, list):
            result = []
            for item in struct:
                result.append(IRTest.make(item))
            return result
        if isinstance(struct, dict):
            result = {}
            for name, value in struct.items():
                result[name] = IRTest.make(value)
            return result

        cls, fields = struct
        if cls == tuple:
            return tuple(IRTest.make(arg) for arg in fields)

        result = cls()
        for name, value in fields.items():
            assert hasattr(result, name)
            setattr(result, name, IRTest.make(value))
        return result

    @staticmethod
    def moduleSelfImport(name):
        return (ImportedEntity, {
            'originModuleName': [name],
            'originName': name,
            'importedName': name
            })

    def assertCorrectParse(self, str, out):
        parsed = self.parse(str)
        correct = IR(self.make(out))
        self.assertEqual(parsed, correct,
            msg="\n  ParsedModule:\n{}\n  NotEqualTo:\n{}"
            .format(parsed, correct))

    def testTrivialModule(self):
        self.assertEqual(IR({}), IR(self.make({})))
        self.assertCorrectParse(
            """module Trivial where""",
            {'Trivial': (Module, {
                'name': 'Trivial',
                'scope': {
                    'Trivial': self.moduleSelfImport('Trivial'),
                }
                })
            })

    # TODO: test import cycles
    def testImportCycles(self):
        #self.assertTrue(False)
        pass

    # TODO: тесты на имя модуля, вложенные модули
    def testModules(self):
        self.assertCorrectParse(
            """module ModuleTest1 where
                 module Submodule1 where
                 module Submodule2 where
            """,
            { 'ModuleTest1': (Module, {
                'name': 'ModuleTest1',
                'scope': {
                    'ModuleTest1': self.moduleSelfImport('ModuleTest1'),
                    'Submodule1': (Module, {
                        'name': 'Submodule1',
                        'scope': {
                            'Submodule1': self.moduleSelfImport('Submodule1'),
                            'ModuleTest1': self.moduleSelfImport('ModuleTest1')
                            }
                        }),
                    'Submodule2': (Module, {
                        'name': 'Submodule2',
                        'scope': {
                            'Submodule2': self.moduleSelfImport('Submodule2'),
                            'ModuleTest1': self.moduleSelfImport('ModuleTest1')
                            }
                        })
                    }
                })
            })

    def testDatatype(self):
        self.assertCorrectParse(
            """module DataTest1 where
               data Empty where
            """,
            { 'DataTest1': (Module, {
                'name': 'DataTest1',
                'scope': {
                    'DataTest1': self.moduleSelfImport('DataTest1'),
                    'Empty': (DataTypeDef, {
                        'name': 'Empty',
                        'typeArgs': [],
                        'constructors': []
                        })
                    }
                })
            })

        self.assertCorrectParse(
            """module DataTest2 where
               
               data Unit where
                 unit : Unit

               data Maybe a where
                 Nothing : Maybe a
                 Just : a → Maybe a
            """,
            { 'DataTest2' (Module, {
                'name': 'DataTest2',
                'scope': {
                    'DataTest2': self.moduleSelfImport('DataTest2'),
                    'Unit': (DataTypeDef, {
                        'name': 'Unit',
                        'typeArgs': [],
                        'constructors': [
                            (Constructor, {
                                'name': 'unit',
                                'type': (Type, {
                                    'typeArgs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Unit'],
                                        'args': [
                                            (TypeExpr, {
                                                'name': ['a'],
                                                'args': []
                                                })
                                        ]
                                        })
                                    })
                                })
                        ]
                        }),

                    'Maybe': (DataTypeDef, {
                        'name': 'Maybe',
                        'typeArgs': 'a',
                        constructors: [
                            (Constructor, {
                                'name': 'Nothing',
                                'type': (Type, {
                                    'typeArgs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Maybe'],
                                        'args': [
                                            (TypeExpr, {
                                                'name': ['a'],
                                                'args': []
                                                })
                                        ]
                                        })
                                    })
                                })
                            (Constructor, {
                                'name': 'Just',
                                'type': (Type, {
                                    'typeArgs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['_→_'],
                                        'args': [
                                            (TypeExpr, {
                                                'name': ['a'],
                                                'args': []
                                                }),
                                            (TypeExpr, {
                                                'name': ['Maybe'],
                                                'args': [
                                                    (TypeExpr, {
                                                        'name': ['a'],
                                                        'args': []
                                                        })
                                                ]
                                                })
                                        ]
                                        })
                                    })
                                })
                        ]
                        })
                    }
                })
            })

    def testFunction(self):
        self.assertCorrectParse(
            """module FunctionTest1 where
               id : a ⇒ a → a
               id x = x

               loop : a ⇒ a
               loop = loop
            """,
            { 'FunctionTest1': (Module, {
                'name': 'FunctionTest1',
                'scope': {
                    'FunctionTest1': self.moduleSelfImport('FunctionTest1'),
                    'id': (FunctionDef, {
                        'name': 'id',
                        'type': (Type, {
                            'typeArgs': ['a'],
                            'typeExpr': (TypeExpr, {
                                'name': ['_→_'],
                                'args': [
                                    (TypeExpr, {
                                        'name': ['a'],
                                        'args': []
                                        }),
                                    (TypeExpr, {
                                        'name': ['a'],
                                        'args': []
                                        })
                                ]
                                })
                            }),
                        'patternLen': 1,
                        'matchRules': [
                            (tuple, (
                                [
                                    (PatternExpr, {
                                        'name': ['x'],
                                        'args': []
                                        })
                                ],
                                (Expr, {
                                    'name': ['x'],
                                    'args': []
                                    })
                                ))
                            ]
                        }),
                    'loop': (FunctionDef, {
                        'name': 'loop',
                        'type': (Type, {
                            'typeArgs': ['a'],
                            'typeExpr': (TypeExpr, {
                                'name': ['a'],
                                'args': []
                                })
                            }),
                        'patternLen': 0,
                        'matchRules': [
                            (tuple, (
                                [],
                                (Expr, {
                                    'name': ['loop'],
                                    'args': []
                                    })
                                ))
                            ]
                        })
                    }
                })
            })

    # TODO: пока пропустить
    def testInvalidDatatype(self):
        pass

    # TODO: пока пропустить
    def testInvalidFunctions(self):
        pass
