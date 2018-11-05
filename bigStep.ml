(* Usa os modulos syntax.ml, env.ml *)
#use "syntax.ml"
#use "env.ml"

exception NoRuleApplies
exception NoOpMatches

let evalBop (op : bop) (v1 : value) (v2 : value) = match op, v1, v2 with
  | Sum, Vint(n1), Vint(n2) -> Vint(n1 + n2)
  | Sub, Vint(n1), Vint(n2) -> Vint(n1 - n2)
  | Mult, Vint(n1), Vint(n2) -> Vint(n1 * n2)
  | Div, Vint(n1), Vint(n2) -> Vint(n1 / n2)
  | And, Vbool(b1), Vbool(b2) -> Vbool (b1 && b2)
  | Or, Vbool(b1), Vbool(b2) -> Vbool (b1 || b2)
  | Eq, Vint(n1), Vint(n2) -> Vbool(n1 = n2)
  | Df, Vint(n1), Vint(n2) -> Vbool(n1 != n2)
  | Lt, Vint(n1), Vint(n2) -> Vbool(n1 < n2)
  | Le, Vint(n1), Vint(n2) -> Vbool(n1 <= n2)
  | Gt, Vint(n1), Vint(n2) -> Vbool(n1 > n2)
  | Ge, Vint(n1), Vint(n2) -> Vbool(n1 >= n2)
  | _ -> raise NoOpMatches


let rec bs (t : expr) (e : env) = match t with
  (* Valores *)
  | Ncte(t') -> Vint(t')    (* BS-NUM *)
  | Bcte(t') -> Vbool(t')    (* BS-BOOL *)
  | Var(x) -> lookup e x  (* BS-ID *)

  (* If *)
  | If(t1,t2,t3) when (bs t1 e = Vbool(true)) -> (* BS-IFTR *)
      let v = bs t2 e in v
  | If(t1,t2,t3) when (bs t1 e = Vbool(false)) -> (* BS-IFFLS *)
      let v = bs t3 e in v

  (* Operadores binarios*)
  | Binop (op, t1, t2) ->
    let t1' = bs t1 e in
    let t2' = bs t2 e in
      evalBop op t1' t2'

  (* funçoes *)


  | _ -> raise NoRuleApplies
