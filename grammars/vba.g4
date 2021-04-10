grammar vba;

startRule: module ;
module : function+ ;
function : FUNCTION ID LPAREN params? RPAREN NEWLINE block NEWLINE END_FUNCTION ;
params : param (COMMA param)*;
param : ID ;
block : statements? ;
statements : statement (NEWLINE statement)? ;
statement
  : ID EQUAL expr  # Statement_Assign
  | expr           # Statement_Expr
  ;
  
expr
  : expr (STAR|SLASH) expr  # Expr_Mul
  | expr (PLUS|MINUS) expr  # Expr_Add
  | LPAREN expr RPAREN   # Expr_Paren
  | INT            # Expr_Int
  | ID             # Expr_Id
  ;
// expr returns [String v]
//   : a=e op='*' b=e  { crate::gen::ty::TY { a: 10, b: 20 }; $v = "* ".to_owned() + $a.v + " " + $b.v; }  # mult
//   | a=e '+' b=e     { $v = "+ ".to_owned() + $a.v + " " + $b.v; }  # add
//   | INT             { $v = $INT.text.to_owned(); }                 # anInt
//   | '(' x=e ')'     { $v = $x.v; }                                 # parens
//   | ID              { $v = $ID.text.to_owned(); }                  # anID
//   ;

FUNCTION : 'Function' ;
END_FUNCTION : 'End Function' ;
NEWLINE : [\r\n\u2028\u2029]+;
ID : 'a'..'z'+ ;
INT : '0'..'9'+ ;
WS : (' '|'\n') -> skip ;
COMMA : ',' ;
STAR : '*' ;
PLUS : '+' ;
MINUS : '-' ;
SLASH : '/' ;
LPAREN : '(' ;
RPAREN : ')' ;
EQUAL : '=' ;
