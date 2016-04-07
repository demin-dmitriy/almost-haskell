// Intentionally extremely oversimplified lexer
// Output of this lexer should be futher filtered and transformed to correctly
// handle identation sensistivity
lexer grammar PreLexer;

// \t, \v and \f tabs are illegal
// (\v is \u000b)
WS : [ ]+ -> skip ;

// Comments
OnelineComment : '--' ~[\r\n]* -> skip ;

BlockComment : '{-' ( ~[{] | '{' ~[-] | BlockComment )*? '-}' -> skip;

// Punctuation
LParen : '(' ;

RParen : ')' ;

NEWLINE : [\r\n] ;

// Reserved keywords
Data : 'data' ;

Where : 'where' ;

Module : 'module' ;

RArrow : '->' | '→' ;

RDoubleArrow : '=>' | '⇒' ;

Equal : '=' ;

Underscore : '_' ;

Colon : ':' ;

// Something crazy like "->=∀x_:⊥]{-→2₂--" is a valid identifier 
// Unfortunately, to overcome limitations of antlr4 we need to explicitly forbid
// identifiers that start with "{-" but still allow identifiers like "{abc" or
// "a{-bc". Also antlr currently doesn't support anything that could make
// this rule a little more concise and readable.
ID :     ~[{ \t\u000b\f\r\n()] LETTER* // ID that doesn't start with '{'
   | '{' ~[- \t\u000b\f\r\n()] LETTER* // ID that starts with '{' not followed by '-'
   ;

fragment LETTER : ~[ \t\u000b\f\r\n()] ;
