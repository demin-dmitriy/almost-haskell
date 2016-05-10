from unittest import TestCase
from AHLexer import AHLexer
from antlr4 import InputStream, CommonTokenStream
from AHParser import AHParser
from IR import *


class IRTest(TestCase):
    def parse(self, s):
        parser = AHParser(CommonTokenStream(AHLexer(InputStream(s))))
        irbuilder = IRBuilder()
        irbuilder.parseModule(parser)
        return irbuilder.makeIR()

    BUILTINS_MODULE = ('builtins.', Module, {
        'name': '{-builtins-}',
        'scope': {
            '{-builtins-}': ('builtins.', ),
            '_→_': ('arrow-type.', DataTypeDef, {
                'name': '_→_',
                'typeArgs': ['a', 'b'],
                'argRefs': [
                    (TypeVariable, {'name': 'a'}),
                    (TypeVariable, {'name': 'b'})
                    ],
                'constructors': []
                })
            }
        })
    ARROW_TYPE_IMPORT = (ImportedEntity, {
        'originModuleName': ['{-builtins-}'],
        'originName': '_→_',
        'importedName': '_→_',
        'entity': ('arrow-type.', )
        })

    # Helper method for defining nested structures, possibly with cyclic
    # backward and forward references.
    @staticmethod
    def make(struct):
        original_refs = {}
        built_refs = {}

        def findRefs(struct):
            if isinstance(struct, list):
                for item in struct: findRefs(item)
            elif isinstance(struct, dict):
                for _, value in struct.items(): findRefs(value)
            elif isinstance(struct, tuple):
                if len(struct) == 1:
                    return
                elif len(struct) == 3: # It's labeled structure
                    label, cls, fields = struct
                    assert label not in original_refs
                    # That was all for this moment
                    # I think, uniplate would be very helpful here
                    original_refs[label] = struct
                else:
                    cls, fields = struct

                if cls is tuple:
                    for arg in fields: findRefs(arg)
                else:
                    for value in fields.values(): findRefs(value)


        def buildStruct(struct):
            if isinstance(struct, (str, int, type(None))):
                return struct
            if isinstance(struct, list):
                return [ buildStruct(item) for item in struct ]
            if isinstance(struct, dict):
                return { name: buildStruct(value)
                         for name, value in struct.items() }

            assert isinstance(struct, tuple)
            if len(struct) == 1: # It's a reference
                label = struct[0]
                if label in original_refs:
                    return buildStruct(original_refs[label])
                elif label in built_refs:
                    return built_refs[label]
                else: assert False
            elif len(struct) == 3: # It's labeled structure
                label, cls, fields = struct
                if label in built_refs:
                    return built_refs[label]
                assert label in original_refs
                result = cls()
                del original_refs[label]
                built_refs[label] = result
            else:
                label = None
                cls, fields = struct
                if cls is tuple:
                    return tuple(buildStruct(arg) for arg in fields)
                result = cls()

            for name, value in fields.items():
                assert hasattr(result, name)
                setattr(result, name, buildStruct(value))
            return result

        findRefs(struct)
        return buildStruct(struct)

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
            {'Trivial': ('top.', Module, {
                'name': 'Trivial',
                'scope': {
                    'Trivial': ('top.', ),
                    '_→_': self.ARROW_TYPE_IMPORT
                }
                }),
             IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })

    # TODO all of this
    # def testImports(self):
    #     self.assertCorrectParse(
    #         """module Imports where

    #         """)
    #     pass

    # # TODO: test import cycles
    # def testImportCycles(self):
    #     #self.assertTrue(False)

    #     # module1 = """module ImportCycle1 where

    #     #           """

    #     # parser = AHParser(CommonTokenStream(AHLexer(InputStream(s))))
    #     # irbuilder = IRBuilder()
    #     # irbuilder.parseModule(parser)

    #     pass

    # TODO: тесты на имя модуля, вложенные модули
    def testModules(self):
        self.assertCorrectParse(
            """module ModuleTest1 where
                 module Submodule1 where
                 module Submodule2 where
            """,
            { 'ModuleTest1': ('top.', Module, {
                'name': 'ModuleTest1',
                'scope': {
                    'ModuleTest1': ('top.', ),
                    'Submodule1': ('sub1.', Module, {
                        'name': 'Submodule1',
                        'scope': {
                            'ModuleTest1': ('top.', ),
                            'Submodule1': ('sub1.', ),
                            'Submodule2': ('sub2.', ),
                            '_→_': self.ARROW_TYPE_IMPORT
                            }
                        }),
                    'Submodule2': ('sub2.', Module, {
                        'name': 'Submodule2',
                        'scope': {
                            'ModuleTest1': ('top.', ),
                            'Submodule1': ('sub1.', ),
                            'Submodule2': ('sub2.', ),
                            '_→_': self.ARROW_TYPE_IMPORT
                            }
                        }),
                    '_→_': self.ARROW_TYPE_IMPORT
                    }
                }),
                IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })

    def testDatatype(self):
        self.assertCorrectParse(
            """module DataTest1 where
               data Empty where
            """,
            { 'DataTest1': ('top.', Module, {
                'name': 'DataTest1',
                'scope': {
                    'DataTest1': ('top.', ),
                    'Empty': (DataTypeDef, {
                        'name': 'Empty',
                        'typeArgs': [],
                        'constructors': []
                        }),
                    '_→_': self.ARROW_TYPE_IMPORT
                    }
                }),
                IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })

        self.assertCorrectParse(
            """module DataTest2 where
               
               data Unit where
                 unit : Unit

               data Maybe a where
                 Nothing : Maybe a
                 Just : a → Maybe a
            """,
            { 'DataTest2': ('top.', Module, {
                'name': 'DataTest2',
                'scope': {
                    'DataTest2': ('top.', ),
                    'Unit': ('unit-type.', DataTypeDef, {
                        'name': 'Unit',
                        'typeArgs': [],
                        'argRefs': [],
                        'constructors': [
                            ('unit-type.unit.', Constructor, {
                                'name': 'unit',
                                'datatype': ('unit-type.', ),
                                'type': (Type, {
                                    'typeArgs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Unit'],
                                        'typeRef': ('unit-type.',),
                                        'args': []
                                        })
                                    })
                                })
                        ]
                        }),
                    'unit' : ('unit-type.unit.', ),

                    'Maybe': ('maybe.', DataTypeDef, {
                        'name': 'Maybe',
                        'typeArgs': ['a'],
                        'argRefs': [('maybe.a.', TypeVariable, {'name': 'a'})],
                        'constructors': [
                            ('maybe.nothing.', Constructor, {
                                'name': 'Nothing',
                                'datatype': ('maybe.', ),
                                'type': (Type, {
                                    'typeArgs': [],
                                    'argRefs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Maybe'],
                                        'typeRef': ('maybe.', ),
                                        'args': [
                                            (TypeExpr, {
                                                'name': ['a'],
                                                'typeRef': ('maybe.a.', ),
                                                'args': []
                                                })
                                        ]
                                        })
                                    })
                                }),
                            ('maybe.just.', Constructor, {
                                'name': 'Just',
                                'datatype': ('maybe.', ),
                                'type': (Type, {
                                    'typeArgs': [],
                                    'argRefs' : [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['_→_'],
                                        'typeRef': ('arrow-type.', ),
                                        'args': [
                                            (TypeExpr, {
                                                'name': ['a'],
                                                'typeRef': ('maybe.a.', ),
                                                'args': []
                                                }),
                                            (TypeExpr, {
                                                'name': ['Maybe'],
                                                'typeRef': ('maybe.', ),
                                                'args': [
                                                    (TypeExpr, {
                                                        'name': ['a'],
                                                        'typeRef': ('maybe.a.', ),
                                                        'args': []
                                                        })
                                                ]
                                                })
                                        ]
                                        })
                                    })
                                })
                        ]
                        }),
                    'Nothing': ('maybe.nothing.', ),
                    'Just': ('maybe.just.', ),
                    '_→_': self.ARROW_TYPE_IMPORT
                    }
                }),
                IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })

    def testFunction(self):
        self.assertCorrectParse(
            """module FunctionTest1 where
               id : a ⇒ a → a
               id x = x

               loop : a ⇒ a
               loop = loop
            """,
            { 'FunctionTest1': ('top.', Module, {
                'name': 'FunctionTest1',
                'scope': {
                    'FunctionTest1': ('top.', ),
                    'id': (FunctionDef, {
                        'name': 'id',
                        'type': (Type, {
                            'typeArgs': ['a'],
                            'argRefs': [
                                ('id.a.', TypeVariable, {'name': 'a'})
                            ],
                            'typeExpr': (TypeExpr, {
                                'name': ['_→_'],
                                'typeRef': ('arrow-type.', ),
                                'args': [
                                    (TypeExpr, {
                                        'name': ['a'],
                                        'typeRef': ('id.a.', ),
                                        'args': []
                                        }),
                                    (TypeExpr, {
                                        'name': ['a'],
                                        'typeRef': ('id.a.', ),
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
                                        'bindingRef': ('id.x.', Variable, {'name': 'x'}),
                                        'args': []
                                        })
                                ],
                                (Expr, {
                                    'name': ['x'],
                                    'funcRef': ('id.x.', ),
                                    'args': []
                                    })
                                ))
                            ]
                        }),
                    'loop': ('loop.', FunctionDef, {
                        'name': 'loop',
                        'type': (Type, {
                            'typeArgs': ['a'],
                            'argRefs': [
                                ('loop.a.', TypeVariable, {'name': 'a'})
                            ],
                            'typeExpr': (TypeExpr, {
                                'name': ['a'],
                                'typeRef': ('loop.a.', ),
                                'args': []
                                })
                            }),
                        'patternLen': 0,
                        'matchRules': [
                            (tuple, (
                                [],
                                (Expr, {
                                    'name': ['loop'],
                                    'funcRef': ('loop.', ),
                                    'args': []
                                    })
                                ))
                            ]
                        }),
                    '_→_': self.ARROW_TYPE_IMPORT
                    }
                }),
                IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })

    def testPatternMatching(self):
        self.assertCorrectParse(
            """module PatternMatchingTest1 where

               data Bool where
                 True : Bool
                 False : Bool

               not : Bool → Bool
               not True = False
               not False = True
            """,
            { 'PatternMatchingTest1': ('top.', Module, {
                'name': 'PatternMatchingTest1',
                'scope': {
                    'PatternMatchingTest1': ('top.', ),
                    'Bool': ('bool.', DataTypeDef, {
                        'name': 'Bool',
                        'typeArgs': [],
                        'argRefs': [],
                        'constructors': [
                            ('bool.true.', Constructor, {
                                'name': 'True',
                                'type': (Type, {
                                    'typeArgs': [],
                                    'argRefs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Bool'],
                                        'typeRef': ('bool.', ),
                                        'args': []
                                        })
                                    }),
                                'datatype': ('bool.', )
                                }),
                            ('bool.false.', Constructor, {
                                'name': 'False',
                                'type': (Type, {
                                    'typeArgs': [],
                                    'argRefs': [],
                                    'typeExpr': (TypeExpr, {
                                        'name': ['Bool'],
                                        'typeRef': ('bool.', ),
                                        'args': []
                                        })
                                    }),
                                'datatype': ('bool.', )
                                })
                            ]
                        }),
                    'True': ('bool.true.', ),
                    'False': ('bool.false.', ),
                    'not': (FunctionDef, {
                        'name': 'not',
                        'type': (Type, {
                            'typeArgs': [],
                            'argRefs': [],
                            'typeExpr': (TypeExpr, {
                                'name': ['_→_'],
                                'typeRef': ('arrow-type.', ),
                                'args': [
                                    (TypeExpr, {
                                        'name': ['Bool'],
                                        'typeRef': ('bool.', ),
                                        'args': []
                                        }),
                                    (TypeExpr, {
                                        'name': ['Bool'],
                                        'typeRef': ('bool.', ),
                                        'args': []
                                        })
                                    ]
                                })
                            }),
                        'patternLen': 1,
                        'matchRules': [
                            (tuple, (
                                [ (PatternExpr,{
                                    'name': ['True'],
                                    'bindingRef': ('bool.true.', ),
                                    'args': []
                                    })
                                ],
                                (Expr, {
                                    'name': ['False'],
                                    'funcRef': ('bool.false.', ),
                                    'args': []
                                    })
                                )),

                            (tuple, (
                                [ (PatternExpr,{
                                    'name': ['False'],
                                    'bindingRef': ('bool.false.', ),
                                    'args': []
                                    })
                                ],
                                (Expr, {
                                    'name': ['True'],
                                    'funcRef': ('bool.true.', ),
                                    'args': []
                                    })
                                ))
                            ]
                        }),
                    '_→_': self.ARROW_TYPE_IMPORT
                    }
                }),
               IR.BUILTINS_MODULE_NAME: self.BUILTINS_MODULE
            })


    # TODO: пока пропустить
    def testInvalidDatatype(self):
        pass

    # TODO: пока пропустить
    def testInvalidFunctions(self):
        pass
