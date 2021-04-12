grammar vba;

startRule: module ;
module : line_end* (function line_end*)+ ;
function : FUNCTION ID LPAREN params? RPAREN line_end+ block line_end+ END_FUNCTION ;
params : param (COMMA param)*;
param : ID ;
block : statements? ;
statements : statement (line_end+ statement)* ;
statement
  : chain_expr EQUAL expr  # Statement_Assign
  | IF expr THEN line_end* block line_end* (ELSE line_end* block line_end*)? END_IF # Statement_If
  | FOR ID EQUAL expr TO expr line_end+ block line_end+ NEXT # Statement_For
  | expr           # Statement_Expr
  ;
  
line_end :
  (COMMENT)? NEWLINE;

expr :
    LPAREN expr RPAREN   # Expr_Paren
  | expr (STAR|SLASH) expr  # Expr_Mul
  | expr (PLUS|MINUS) expr  # Expr_Add
  | expr (GEQ|GT|LEQ|LT|EQUAL|NEQ) expr # Expr_Comp
  | expr (AND|OR) expr # Expr_Logical_Comb
  | expr (CONCAT) expr # Expr_Concat
  | INT             # Expr_Int
  | STRINGLITERAL   # Expr_String
  | chain_expr      # Expr_Chain
  ;

chain_expr :
    chain_expr PERIOD app_expr  # Chain_expr_chain
  | app_expr                    # Chain_expr_base
  ;

app_expr : ID (LPAREN arguments? RPAREN)? ;    
  
arguments
  : expr (COMMA expr)*;
  
// expr returns [String v]
//   : a=e op='*' b=e  { crate::gen::ty::TY { a: 10, b: 20 }; $v = "* ".to_owned() + $a.v + " " + $b.v; }  # mult
//   | a=e '+' b=e     { $v = "+ ".to_owned() + $a.v + " " + $b.v; }  # add
//   | INT             { $v = $INT.text.to_owned(); }                 # anInt
//   | '(' x=e ')'     { $v = $x.v; }                                 # parens
//   | ID              { $v = $ID.text.to_owned(); }                  # anID
//   ;

COMMENT : SINGLEQUOTE (~[\r\n\u2028\u2029])*;
SINGLEQUOTE : '\'' ;

GEQ : '>=';
GT : '>';
LEQ : '<=';
LT : '<';
NEQ : '<>';
STRINGLITERAL : '"' (~["\r\n] | '""')* '"';
PERIOD: '.' ;
FUNCTION : 'Function' ;
END_FUNCTION : 'End Function' ;
NEWLINE : [\r\n\u2028\u2029]+;
IF : 'If' ;
THEN : 'Then' ;
ELSE : 'Else' ;
END_IF : 'End If';
FOR : 'For' ;
TO : 'To' ;
NEXT : 'Next' ;
INT : '0'..'9'+ ;
WS : (' ')+ -> skip ;
COMMA : ',' ;
STAR : '*' ;
PLUS : '+' ;
MINUS : '-' ;
SLASH : '/' ;
LPAREN : '(' ;
RPAREN : ')' ;
EQUAL : '=' ;
AND : 'And';
OR : 'Or' ;
CONCAT : '&' ;
ID : ('a'..'z'|'A'..'Z')+ ;