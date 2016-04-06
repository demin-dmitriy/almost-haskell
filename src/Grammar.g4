parser grammar Grammar;

options { tokenVocab=Lexer; }

moduleHeader : 'module' ID 'where' ;

topLevelModule :
    moduleHeader
    ( BeginBlock EndBlock definitions
    | BeginBlock definitions EndBlock
    ) EOF;

definitions : ( module | dataDef | funcDef | funcTypeDef )* ;

module : moduleHeader BeginBlock definitions EndBlock ;

dataDef : 'data' ID typeParams 'where' BeginBlock constructorDef* EndBlock ;

// Same as funcTypeDef currently
constructorDef : ID ':' ( typeParams '=>' )? typeExpr NEWLINE ;

funcTypeDef : ID ':' ( typeParams '=>' )? typeExpr NEWLINE ;

funcDef : pattern '=' expr NEWLINE ;

typeExpr :
    concreteType
    | <assoc=right> typeExpr '->' typeExpr
    | '(' typeExpr ')'
    ;

typeParams : ID* ;

concreteType : ID typeArg* ;

typeArg : ID | '(' typeExpr ')' ;

pattern : ID patternArg* ;

patternArg : ID | '_' | '(' pattern ')' ;

expr : functionExpr ;

functionExpr : ID funcArg* ;

funcArg : ID | '(' expr ')' ;
