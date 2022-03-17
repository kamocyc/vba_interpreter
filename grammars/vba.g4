grammar vba;

startRule: module EOF ;
module : line_end* ((function | procedure | property_let | module_variable_declaration) line_end*)+ ;
function : function_modifier? FUNCTION ID LPAREN params? RPAREN (AS type_name)? line_end+ block line_end+ END_FUNCTION ;
procedure : function_modifier? SUB ID LPAREN params? RPAREN line_end+ block line_end+ END_SUB ;
property_let : function_modifier? PROPERTY_LET ID LPAREN param RPAREN line_end+ block line_end+ END_PROPERTY ;
params : param (COMMA param)*;
param : ID (AS type_name)?;
function_modifier :
    PUBLIC
  | PRIVATE
;

block : statements? ;
statements : statement (line_end+ statement)* ;
statement
  : chain_expr EQUAL expr  # Statement_Assign
  | SET chain_expr EQUAL expr  # Statement_SetAssign
  | IF expr THEN line_end* block line_end* (ELSE line_end* block line_end*)? END_IF # Statement_If
  | FOR ID EQUAL expr TO expr line_end+ block line_end+ NEXT ID? # Statement_For
  | expr           # Statement_Expr
  | chain_expr arguments   # Statement_Call
  | variable_declaration ID (AS type_name)?  # Statement_variable_declaration
  ;

module_variable_declaration :
    variable_declaration ID (AS type_name)?
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
  | NEW ID          # Expr_New
  ;

chain_expr :
    chain_expr PERIOD app_expr  # Chain_expr_chain
  | app_expr                    # Chain_expr_base
  ;

app_expr : ID (LPAREN arguments? RPAREN)? ;    
  
arguments
  : expr (COMMA expr)*;

variable_declaration :
      DIM
    | PUBLIC
    | PRIVATE
;

type_name :
      TYPE_INT       # Typename_Int
    | TYPE_STRING    # Typename_String
    | TYPE_VARIANT   # Typename_Variant
    | TYPE_BOOLEAN   # Typename_Boolean
    | TYPE_OBJECT    # Typename_Object
    | ID             # Typename_Id
;

COMMENT : (SINGLEQUOTE | REM) (~[\r\n\u2028\u2029])*;
SINGLEQUOTE : '\'' ;

PROPERTY_LET : 'Property Let' ;
END_PROPERTY : 'End Property' ;
REM : 'Rem' ;
NEW : 'New' ;
GEQ : '>=';
GT : '>';
LEQ : '<=';
LT : '<';
NEQ : '<>';
STRINGLITERAL : '"' (~["\r\n] | '""')* '"';
PERIOD: '.' ;
FUNCTION : 'Function' ;
SUB : 'Sub' ;
END_SUB : 'End Sub' ;
PUBLIC : 'Public' ;
PRIVATE : 'Private' ;
END_FUNCTION : 'End Function' ;
NEWLINE : [\r\n\u2028\u2029]+;
IF : 'If' ;
DIM : 'Dim' ;
AS : 'As' ;
SET : 'Set' ;
TYPE_STRING : 'String' ;
TYPE_INT : 'Integer' ;
TYPE_VARIANT : 'Variant' ;
TYPE_BOOLEAN : 'Boolean' ;
TYPE_OBJECT  : 'Object' ;
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
ID : ('a'..'z'|'A'..'Z')('a'..'z'|'A'..'Z'|'0'..'9'|'_')* ;