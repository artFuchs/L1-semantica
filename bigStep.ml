(* Usa os modulos syntax.ml, env.ml *)

#use "syntax.ml"
#use "env.ml"


exception NoRuleApplies
exception NoOpMatches
exception InvalidApp
exception InvalidList
exception EmptyList


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

  (* Condicional *)
  | If(t1,t2,t3) when (bs t1 e = Vbool(true)) -> (* BS-IFTR *)
      let v = bs t2 e in v
  | If(t1,t2,t3) when (bs t1 e = Vbool(false)) -> (* BS-IFFLS *)
      let v = bs t3 e in v

  (* Operadores unários*)
  | Unop (Not, t) ->
      let v = bs t e in
      ( match v with
        | Vbool(vb) -> Vbool(not vb)
        | _ -> raise NoOpMatches
      )

  (* Operadores binários*)
  | Binop (op, t1, t2) ->
    let t1' = bs t1 e in
    let t2' = bs t2 e in
    evalBop op t1' t2'

  (* funções *)
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

  (* Exceções para a Aplicação de funcoes *)
  | App(Raise, _) -> Vraise
  | App(_, Raise) -> Vraise
  (* Aplicação de funcoes *)
  | App (t1, t2) ->
    let cl = bs t1 e in
    let v' = bs t2 e in
    ( match cl with
      | Vclos(var, expr, e') -> (* BS-APP *)
        let e'' = updateEnv e' var v' in
        let v = bs expr e'' in v
      | Vrclos(f, var, expr, e') -> (* BS-APPREC *)
        let e'' = updateEnv e' f (Vrclos(f, var, expr, e')) in
        let e''' = updateEnv e'' var v' in
        let v = bs expr e''' in v
      | _ -> raise InvalidApp
    )

  (* Pares *)
  | Pair(t1, t2) -> Vpair(bs t1 e, bs t2 e)
  | Hd( Pair(t, _)) -> bs t e
  | Tl( Pair(_, t)) -> bs t e

  (* Listas *)
  | Nil -> Vnil
  | Cons(t1, Nil) -> Vlist([bs t1 e; Vnil])
  | Cons(t1, t2) ->
      ( match (bs t2 e) with
        | Vlist(tl) -> Vlist((bs t1 e)::tl)
        | _ -> raise InvalidList
      )
  | IsEmpty(Nil) -> Vbool(true)
  | IsEmpty(Cons(t1,t2)) -> Vbool(false)
  | Hd(t) ->
      let l = bs t e in
      ( match l with
        | Vlist(hd :: tl) -> hd
        | Vnil -> raise EmptyList
        | _ -> raise InvalidList
      )
  | Tl(t) ->
      let l = bs t e in
      ( match l with
        | Vlist(hd :: tl) -> Vlist(tl)
        | Vnil -> raise EmptyList
        | _ -> raise InvalidList
      )

  (* Tratamento de Exceções *)
  | Raise -> Vraise
  | If (Raise,_,_) -> Vraise
  | Try(t1, t2) ->
      let t1' = bs t1 e in
      if t1' = Vraise
      then bs t2 e
      else t1'

  (* Expressão errada *)
  | _ -> raise NoRuleApplies
