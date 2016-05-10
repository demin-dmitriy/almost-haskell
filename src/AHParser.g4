parser grammar AHParser;

options { tokenVocab=AHLexer; }

moduleHeader : 'module' ID 'where' ;

topLevelModule :
    moduleHeader
    ( BeginBlock EndBlock definitions
    | BeginBlock definitions EndBlock
    ) EOF;

definitions : ( module | dataDef | funcDef | funcTypeDef | importStmt /*| exportStmt */)* ;

module : moduleHeader BeginBlock definitions EndBlock ;

dataDef : 'data' ID typeParams 'where' BeginBlock constructorDef* EndBlock ;

// Same as funcTypeDef currently
constructorDef : ID ':' typeWithArgs NEWLINE ;

funcTypeDef : ID ':' typeWithArgs NEWLINE ;

typeWithArgs : ( typeParams '=>' )? typeExpr ;

funcDef : ID pattern* '=' expr NEWLINE ;

importStmt : moduleImport | entityImport ;

moduleImport : 'import' qualifiedName (ID ID)? NEWLINE ;

entityImport : 'from' qualifiedName 'import' importedName* NEWLINE ;

// This is probably isn't needed because it can be emulated already using
// multiple modules
// exportStmt : 'export' ... NEWLINE ;

// Second ID in second case is actuallly 'as' but isn't used as keyword here.
// It should be checked programmatically)
importedName : ID               # preserveNameImport 
             | '(' ID ID ID ')' # renamingImport
             ;

typeExpr :
    concreteType                           # concrTypeExpr
    | <assoc=right> typeExpr '->' typeExpr # arrowTypeExpr
    | '(' typeExpr ')'                     # parenTypeExpr
    ;

typeParams : ID* ;

concreteType : qualifiedName typeArg* ;

typeArg : qualifiedName    # nameTypeArg
        | '(' typeExpr ')' # parenTypeArg
        ;

pattern : qualifiedName       # namePattern
        | '_'                 # placeHolderPattern
        | '(' patternExpr ')' # parenPattern
        ;

patternExpr : qualifiedName pattern*       # normalPatternExpr
            | '(' patternExpr ')' pattern* # parenPatternExpr
            ;

// There may be some other types of expressions in future
expr : functionExpr ;

// TODO: This doesn't look too pretty. Maybe rewrite in a standard way?
// It works, because we don't have lambdas
functionExpr : qualifiedName funcArg*        # normalFunctionExpr
             | '(' functionExpr ')' funcArg* # parenFunctionExpr ;

funcArg : qualifiedName # nameFuncArg
        | '(' expr ')'  # parenFuncArg
        ;

qualifiedName : ID ('.' ID)* ;
