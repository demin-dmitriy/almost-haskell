parser grammar AHParser;

options { tokenVocab=Lexer; }

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
constructorDef : ID ':' ( typeParams '=>' )? typeExpr NEWLINE ;

funcTypeDef : ID ':' ( typeParams '=>' )? typeExpr NEWLINE ;

funcDef : pattern '=' expr NEWLINE ;

importStmt : moduleImport | entityImport ;

moduleImport : 'import' importedName  NEWLINE ;

entityImport : 'from' qualifiedName 'import' importedName* NEWLINE ;

// This is probably isn't needed because it can be emulated already using
// multiple modules
// exportStmt : 'export' ... NEWLINE ;

// Second ID is actuallly 'as' but isn't used as keyword here.
// It shoul be checked programmatically)
importedName : ID (ID  ID)? ;

typeExpr :
    concreteType
    | <assoc=right> typeExpr '->' typeExpr
    | '(' typeExpr ')'
    ;

typeParams : ID* ;

concreteType : qualifiedName typeArg* ;

typeArg : qualifiedName | '(' typeExpr ')' ;

pattern : qualifiedName patternArg* ;

patternArg : qualifiedName | '_' | '(' pattern ')' ;

// There may be some other types of expression in future
expr : functionExpr ;

functionExpr : qualifiedName funcArg* ;

funcArg : qualifiedName | '(' expr ')' ;

qualifiedName : ID ('.' ID)* ;
