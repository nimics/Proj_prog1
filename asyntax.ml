(*

prend l'AST, check si il est syntaxiquement correct

*)

type ast =
  | F_Int of int
  | F_Float of float
  | Int of ast
  | Float of ast
  | Plus of ast
  | Minus of ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Mod of ast * ast
  | Exp of ast * ast
  | Adddot of ast * ast
  | Subdot of ast * ast
  | Muldot of ast * ast
;;

type ty = F | I ;;

let rec check arbre = match arbre with
  | F_Int(_) -> I
  | F_Float(_) -> F
  | Int(arbre) -> assert (check arbre = F); I
  | Float(arbre) -> assert (check arbre = I); F
  | Plus(arbre)
  | Minus(arbre) -> assert (check arbre = I); I
  | Add(g, d)
  | Sub(g, d)
  | Mul(g, d)
  | Div(g, d)
  | Mod(g, d) -> assert (check g = I && check d = I); I
  | Exp(g, d) -> assert (check g = I && check d = I); I
  | Adddot(g, d)
  | Subdot(g, d)
  | Muldot(g, d) -> assert (check g = F && check d = F); F
;;
