# Production rules
program         -> statement* EOF ;
statement       -> printStmt | exprStmt ;
exprStmt        -> expression ";" ;
printStmt       -> "print" expression ";" ;
expression      -> equality ;
equality        -> comparison (( "==" | "!=" ) comparison )* ;
comparison      -> term (( ">" | "<" | ">=" | "<=" ) term )* ;
term            -> factor (( "+" | "-" ) factor )* ;
factor          -> unary (( "*" | "/" ) unary )* ;
unary           -> ( "-" | "!" ) unary | primary ;
primary         -> NUMBER | STRING | "true" | "false" | "null" | "(" expression ")" ;

# Associativity
Name	    Operators	Associates
Equality	== !=	    Left
Comparison	> >= < <=	Left
Term	    - +	        Left
Factor	    / *	        Left
Unary	    ! -	        Right
