# Production rules
program         -> declaration* EOF ;
declaration     -> varDecl | statement ;
varDecl         -> "let" IDENTIFIER ( "=" expression )? ";" ;
statement       -> printStmt | exprStmt | block | ifStmt | WhileLoop ;
whileLoop       -> "while" "expression" block ;
forLoop         -> "for" "(" ( varDecl )? ";" expression ";" ( statement )? ")" block ;
ifStmt          -> "if" expression block ( "else" block )? ;
block           -> "{" declaration * "}" ;
exprStmt        -> expression ";" ;
printStmt       -> "print" expression ";" ;
expression      -> assignment ;
assignment      -> IDENTIFIER "=" assignment | logical_or ;
logical_or      -> logical_and ( "or" logical_and )* ;
logical_and     -> equality ( "and" equality )* ;
equality        -> comparison (( "==" | "!=" ) comparison )* ;
comparison      -> term (( ">" | "<" | ">=" | "<=" ) term )* ;
term            -> factor (( "+" | "-" ) factor )* ;
factor          -> unary (( "*" | "/" ) unary )* ;
unary           -> ( "-" | "!" ) unary | primary ;
primary         -> NUMBER | STRING | "true" | "false" | "null" | "(" expression ")" | IDENTIFIER ;
NUMBER          -> [0-9]+ ;
STRING          -> [a-z_]+ ;
IDENTIFIER      -> [a-z_]+ ;

# Associativity
Name	    Operators	Associates
Equality	== !=	    Left
Comparison	> >= < <=	Left
Term	    - +	        Left
Factor	    / *	        Left
Unary	    ! -	        Right
