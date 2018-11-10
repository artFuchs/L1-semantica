(* Usa os modulos syntax.ml, env.ml *)
#use "syntax.ml"
#use "env.ml"

exception NoRuleApplies
exception NoOpMatches
exception InvalidApp


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

  (* Operadores unarios*)
  | Unop (Not, t) ->
      let v = bs t e in
      ( match v with
      | Vbool(vb) -> Vbool(not vb)
      | _ -> raise NoOpMatches)

  (* Operadores binarios*)
  | Binop (op, t1, t2) ->
    let t1' = bs t1 e in
    let t2' = bs t2 e in
    evalBop op t1' t2'

  (* funcoes *)
  | Fn(var, t') -> (* BS-FN *)
    Vclos(var, t', e)
  | Let(var, t1, t2) -> (* BS-LET *)
    let v' = bs t1 e in
    let e' = updateEnv e var v' in
    let v = bs t2 e' in v
  | Lrec(f,var,t1,t2) -> (* BS-LETREC *)
    let v' = Vrclos(f,var,t1,e) in
    let e' = updateEnv e f v' in
    let v = bs t2 e' in v

  (* Applicacao de funcoes *)
  | App(t1, t2) ->
    let cl = bs t1 e in
    let v' = bs t2 e in match cl with
      | Vclos(var, expr, e') -> (* BS-APP *)
        let e'' = updateEnv e' var v' in
        let v = bs expr e'' in v
      | Vrclos(f, var, expr, e') -> (* BS-APPREC *)
        let e'' = updateEnv e' f (Vrclos(f, var, expr, e')) in
        let e''' = updateEnv e'' var v' in
        let v = bs expr e''' in v
      | _ -> raise InvalidApp

  | _ -> raise NoRuleApplies
