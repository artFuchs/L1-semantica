#use "syntax.ml"
#use "env.ml"

exception NoRuleApplies
type tyEq = tipo * tipo

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


let rec unify subst (const : tyEq list) = [];
