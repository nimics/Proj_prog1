   {
        open Parser
        exception Eof
    }

rule token = parse
    | [' ' '\t' '\n']+
        { token lexbuf }    (* skip les blancs *)
    | "("
        { LBRACK }
    | ")"
        { RBRACK }
    | ['0'-'9']++ '.' ['0'-'9']** as s
        { VARFLOAT(float_of_string s) }
    | ['0'-'9']++ as s
        { VARINT(int_of_string s) }
    | "int"
        { INT }
    | "float"
        { FLOAT }
    | "+"
        { PLUS }
    | "+."
        { PLUSDOT }
    | "-"
        { MINUS }
    | "-."
        { MINUSDOT }
    | "*"
        { MUL }
    | "*."
        { MULDOT }
    | "/"
        { DIV }
    | "%"
        { MOD }
    | "^"
        { EXP }
    | _ as c
        { failwith ("caractère illégal : " ^ String.make 1 c) }
    | eof
        { Teof }