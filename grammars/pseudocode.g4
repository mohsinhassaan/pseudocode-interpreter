grammar pseudocode;

prog: line+ EOF;

line: (statement COMMENT? | COMMENT);

statement:
	pass
	| ifstmt
	| casestmt
	| forstmt
	| whilestmt
	| repeatstmt
	| funcstmt
	| procstmt
	| callstmt
	| inputstmt
	| printstmt
	| constdecl
	| letstmt
	| vardecl
	| arraydecl
	| expression;

pass: PASS;

<<<<<<< HEAD
constdecl: CONSTANT varname EQ expression;
=======
constdecl: CONSTANT varname EQ literal;
>>>>>>> main

vardecl: DECLARE varname COLON type;

arraydecl: DECLARE varname COLON (oneDimArray | twoDimArray);

oneDimArray: ARRAY LBRACKET range RBRACKET OF type;

twoDimArray: ARRAY LBRACKET range COMMA range RBRACKET OF type;

range: intLiteral COLON intLiteral;

printstmt: OUTPUT printlist?;

printlist: expression (COMMA expression?)*;

letstmt: LET? variableassignment;

variableassignment: var LARROW expression;

casestmt:
	CASE OF var (caseclause | rangeClause)+ otherwiseClause? endcasestmt;

caseclause: expression COLON statement+;

rangeClause: expression (TO literal)? COLON statement+;

otherwiseClause: OTHERWISE COLON statement+;

endcasestmt: ENDCASE;

ifstmt: IF expression thenstmt elsestmt? endifstmt;

thenstmt: THEN statement+;

elsestmt: ELSE statement+;

endifstmt: ENDIF;

forstmt: FOR variableassignment to step? statement+ nextstmt;

to: TO expression;

step: STEP expression;

nextstmt: NEXT varlist?;

whilestmt: WHILE expression statement+ endwhilestmt;

endwhilestmt: ENDWHILE;

repeatstmt: REPEAT statement+ untilstmt;

untilstmt: UNTIL expression;

inputstmt: INPUT varname;

callstmt: CALL varname (LPAREN arglist? RPAREN)?;

funcstmt:
	FUNCTION varname (LPAREN arglist? RPAREN) RETURNS type (
		statement
		| returnstmt
	)+ endfuncstmt;

endfuncstmt: ENDFUNCTION;

procstmt:
	PROCEDURE varname (LPAREN arglist? RPAREN)? statement+ endprocstmt;

endprocstmt: ENDPROCEDURE;

returnstmt: RETURN expression;

// expressions and such
func: funcCall | var | (LPAREN expression RPAREN) | literal;

funcCall: varname LPAREN exprlist? RPAREN;

arrayaccess:
	varname LBRACKET expression RBRACKET
	| varname LBRACKET expression RBRACKET;

literal:
	strLiteral
	| charLiteral
	| intLiteral
	| realLiteral
	| boolLiteral;

strLiteral: STRINGLITERAL;

intLiteral: INTEGERLITERAL;

realLiteral: REALLITERAL;

boolLiteral: BOOLEANLITERAL;

charLiteral: CHARLITERAL;

signExpression: NOT? (PLUS | MINUS)? func;

multiplyingExpression:
	signExpression ((TIMES | DIV | MOD) signExpression)*;

addingExpression:
	multiplyingExpression ((PLUS | MINUS) multiplyingExpression)*;

relationalExpression:
	addingExpression (RELOP addingExpression)?
	| BOOLEANLITERAL;

expression:
	func
	| (relationalExpression ((AND | OR) relationalExpression)*);

var: varname | arrayaccess;

varname: VARNAME;

varlist: (var (COMMA var)*);

exprlist: expression (COMMA expression)*;

arglist: var COLON type (COMMA var COLON type)*;

type: INTEGER | REAL | CHAR | STRING | BOOLEAN;

PASS: 'PASS';

CONSTANT: 'CONSTANT';

DECLARE: 'DECLARE';

RETURN: 'RETURN';

OUTPUT: 'OUTPUT';

IF: 'IF';

NEXT: 'NEXT';

THEN: 'THEN';

ELSE: 'ELSE';

ENDIF: 'ENDIF';

CASE: 'CASE';

OF: 'OF';

OTHERWISE: 'OTHERWISE';

ENDCASE: 'ENDCASE';

LPAREN: '(';

RPAREN: ')';

LBRACKET: '[';

RBRACKET: ']';

PLUS: '+';

MINUS: '-';

TIMES: '*';

DIV: '/';

MOD: 'MOD';

NEQ: LT GT;

RELOP: GTE | LTE | NEQ | EQ | GT | LT;

GTE: GT EQ;

LTE: LT EQ;

GT: '>';

LT: '<';

COMMA: ',';

LET: 'LET';

EQ: '=';

LARROW: '<-';

FOR: 'FOR';

TO: 'TO';

STEP: 'STEP';

WHILE: 'WHILE';

DO: 'DO';

ENDWHILE: 'ENDWHILE';

REPEAT: 'REPEAT';

UNTIL: 'UNTIL';

INPUT: 'INPUT';

COLON: ':';

UNDERSCORE: '_';

CALL: 'CALL';

AND: 'AND';

OR: 'OR';

DEF: 'DEF';

FUNCTION: 'FUNCTION';

ENDFUNCTION: 'ENDFUNCTION';

PROCEDURE: 'PROCEDURE';

ENDPROCEDURE: 'ENDPROCEDURE';

RETURNS: 'RETURNS';

ARRAY: 'ARRAY';

NOT: 'NOT';

INTEGER: 'INTEGER';

REAL: 'REAL';

CHAR: 'CHAR';

STRING: 'STRING';

BOOLEAN: 'BOOLEAN';

COMMENT: '//' ~[\r\n]* -> skip;

STRINGLITERAL: '"' ~ ["\r\n]* '"';
CHARLITERAL: '\'' ('\\' [rntf\\] | ~ ['\r\n\t\f]*) '\'';
INTEGERLITERAL: SIGN? ('0' .. '9')+;
REALLITERAL: SIGN? ('0' .. '9')* '.' ('0' .. '9')+;
BOOLEANLITERAL: 'TRUE' | 'FALSE';
SIGN: '+' | '-';
VARNAME: (LETTERS | UNDERSCORE) (LETTERS | UNDERSCORE | NUMBERS)*;
LETTERS: ('a' .. 'z' | 'A' .. 'Z')+;
NUMBERS: ('0' .. '9')+;
WS: [ \r\n\t]+ -> channel (HIDDEN);
