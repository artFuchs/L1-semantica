#use "syntax.ml"
#use "env.ml"

exception NoRuleApplies
exception Fail
type tyEq = tipo * tipo
type subst = variable * tipo

let rec collect (env : tEnv) (program : expr) : tipo * tyEq list = match program with
    (* Valores *)
    | Ncte(t) -> (Tint, [])
    | Bcte(t) -> (Tbool, [])

    (* Operadores *)
    | Unop(Not, t) ->
      let (ty, c) = collect env t in
      (Tbool, List.append c [(ty,Tbool)])
    | Binop(op, t1, t2) when (op = Sum || op = Sub || op = Mult || op = Div) ->
      let (ty1, c1) = collect env t1 in
      let (ty2, c2) = collect env t2 in
      (Tint, List.concat [c1; c2; [(ty1, Tint); (ty2, Tint)] ])
    | Binop(op, t1, t2) when (op = And || op = Or)->
      let (ty1, c1) = collect env t1 in
      let (ty2, c2) = collect env t2 in
      (Tbool, List.concat [c1; c2; [(ty1, Tbool); (ty2, Tbool)] ])
    | Binop(op, t1, t2) when (op = Eq || op = Df || op = Lt || op = Le || op = Gt || op = Ge ) ->
      let (ty1, c1) = collect env t1 in
      let (ty2, c2) = collect env t2 in
      (Tbool, List.concat [c1; c2; [(ty1, Tint); (ty2, Tint)] ])
    | Pair(t1,t2) ->
      let (ty1, c1) = collect env t1 in
      let (ty2, c2) = collect env t2 in
      (Tpair(ty1,ty2), List.concat [c1; c2])
    | _ -> raise NoRuleApplies


let rec unify (substs : subst list) (consts : tyEq list) = match consts with
    | [] -> substs
    | (Tint, Tint) :: c -> unify substs c
    | (Tbool, Tbool) :: c -> unify substs c
    | (Tlist(t1), Tlist(t2) ) :: c -> unify substs ( (t1,t2) :: c )
    | (Tfn(t1,t2), Tfn(t3,t4) ) :: c -> unify substs ( (t1,t3) :: (t2,t4) :: c )
    | (Tpair(t1,t2), Tpair(t3,t4) ) :: c -> unify substs ( (t1,t3) :: (t2,t4) :: c )
    | (Tvar(X), Tvar(X)) :: c -> unify substs c
    | _ -> raise NoRuleApplies
