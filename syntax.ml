(* Sintaxe retirada do moodle *)
(* Outros operadores binário e unários podem ser adicionados a linguagem *)

type bop = Sum   (* + *)
         | Sub (* - *)
         | Mult  (* x *)
         | Div   (* / *)
         | And   (* and *)
         | Or    (* or *)
         | Eq    (* = *)
         | Df    (* <> *)
         | Lt    (*  < *)
         | Le    (* <= *)
         | Gt    (* > *)
         | Ge    (* >= *)

type uop = Not

type variable =  string

type expr = Ncte of int
          | Bcte of bool
          | Binop of bop * expr * expr
          | Unop of uop * expr
          | Pair of expr * expr
          | If of expr * expr * expr
          | Var of variable
          | App of expr * expr
          | Fn of variable * expr
          | Let of variable * expr * expr
          | Lrec of variable *  variable * expr * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr
