%token LBRACK
%token RBRACK
%token <int> VARINT
%token <float> VARFLOAT
%token INT
%token FLOAT
%token PLUS
%token PLUSDOT
%token MINUS
%token MINUSDOT
%token MUL
%token MULDOT
%token DIV
%token MOD
%token EXP
%token Teof

/* def la précédence */

%left PLUS PLUSDOT MINUS MINUSDOT
%left MUL MULDOT DIV
%left MOD

%type <Asyntax.ast> main
%type <Asyntax.ast> expr
%start main


%%
    main:
        expr Teof                { $1 }
    expr:
        | VARINT
            { F_Int($1) }
        | VARFLOAT
            { F_Float($1)}
        | LBRACK expr RBRACK
            { $2 }
        | INT LBRACK expr RBRACK
            { Int($3) }
        | FLOAT LBRACK expr RBRACK
            { Float($3) }
        | expr PLUS expr
            { Add($1, $3) }
        | PLUS expr
            { Plus($2) }
        | expr PLUSDOT expr
            { Adddot($1, $3) }
        | expr MINUS expr
            { Sub($1, $3) }
        | MINUS LBRACK expr RBRACK
            { Minus($3) }
        | MINUS VARINT
            { Minus(F_Int( $2 )) }
        | expr MINUSDOT expr
            { Subdot($1, $3) }
        | expr MUL expr
            { Mul($1, $3) }
        | expr MULDOT expr
            { Muldot($1, $3) }
        | expr DIV expr
            { Div($1, $3) }
        | expr MOD expr
            { Mod($1, $3) }
        | expr EXP expr
            { Exp($1, $3) }