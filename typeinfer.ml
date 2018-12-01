(* Usa os modulos syntax.ml, env.ml
#use "syntax.ml"
#use "env.ml"
open Syntax
open Env *)

exception CollectFail
exception UnifyFail
exception TypeinferFail
type tyEq = tipo * tipo
type replacement = variable * tipo

let newX lastX =
  let a = (int_of_string lastX) + 1 in
  string_of_int a


(* collectR - função auxiliar de collect.

  Uso: collectR env program lastX
    env : tEnv - ambiente de tipos do programa
    program : expr - uma expressão que representa o programa a ser verificado
    lastX : string - a ultima variável de tipo criada até agora pelo coletor
  retorna : (T, C) onde:
    T : tipo - tipo encontrado ao analisar o programa
    C : tyEq list - lista de equações de tipo a serem resolvidas para determinar o tipo do programa
  pode gerar exceção UnifyFail
*)
let rec collectR (env : tEnv) (program : expr) (lastX : string): tipo * tyEq list * variable = match program with
    (* Valores *)
    | Ncte(t) -> (Tint, [], lastX)
    | Bcte(t) -> (Tbool, [], lastX)

    (* Operadores *)
    | Unop(Not, t) ->
      let (ty, c, acX) = collectR env t lastX in
      (Tbool, List.concat [c; [(ty,Tbool)] ], acX)
    | Binop(op, t1, t2) when (op = Sum || op = Sub || op = Mult || op = Div) ->
      let (ty1, c1, acX1) = collectR env t1 lastX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      (Tint, List.concat [c1; c2; [(ty1, Tint); (ty2, Tint)] ], acX2)
    | Binop(op, t1, t2) when (op = And || op = Or)->
      let (ty1, c1, acX1) = collectR env t1 lastX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      (Tbool, List.concat [c1; c2; [(ty1, Tbool); (ty2, Tbool)] ], acX2)
    | Binop(op, t1, t2) when (op = Eq || op = Df || op = Lt || op = Le || op = Gt || op = Ge ) ->
      let (ty1, c1, acX1) = collectR env t1 lastX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      (Tbool, List.concat [c1; c2; [(ty1, Tint); (ty2, Tint)] ], acX2)
    | Pair(t1,t2) ->
      let (ty1, c1, acX1) = collectR env t1 lastX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      (Tpair(ty1,ty2), List.concat [c1; c2], acX2)
    | If(t1,t2,t3) ->
      let (ty1, c1, acX1) = collectR env t1 lastX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      let (ty3, c3, acX3) = collectR env t3 acX2  in
      (ty2,List.concat [c1; c2; c3; [(ty1,Tbool); (ty2,ty3)] ], acX3)
    | Var(x) ->
      let t = lookup env x
      in (t, [], lastX)
    | App(t1, t2) ->
      let nX = newX lastX in
      let (ty1, c1, acX1) = collectR env t1 nX in
      let (ty2, c2, acX2) = collectR env t2 acX1 in
      let tX = Tvar nX in
      (tX, List.concat [c1; c2; [(ty1,Tfn(ty2,tX) )] ], acX2)
    | Fn(x, t) ->
      let nX = newX lastX in
      let tX = Tvar nX in
      let env' = updateTEnv env x tX in
      let (ty, c, acX) = collectR env' t nX in
      (Tfn(tX, ty), c, acX)
    | Let(x,t1,t2) ->
      let nX = newX lastX in
      let tX = Tvar nX in
      let env' = updateTEnv env x tX in
      let (ty1, c1, acX1) = collectR env t1 nX in
      let (ty2, c2, acX2) = collectR env' t2 acX1 in
      (ty2, List.concat [c1 ; c2; [(tX, ty1)] ], acX2)
    | Lrec(f,y,t1,t2) ->
      let fX = newX lastX in
      let yX = newX fX in
      let tF = Tvar fX in
      let tY = Tvar yX in
      let envF = updateTEnv env f tF in
      let envFY = updateTEnv envF y tY in
      let (ty1, c1, acX1) = collectR envFY t1 yX in
      let (ty2, c2, acX2) = collectR envF t2 acX1 in
      (ty2, List.concat [c1; c2; [(tF,Tfn(tY,ty1))]], acX2)
    | Nil -> (Tlist(Tvar (newX lastX)), [], lastX)
    | Cons(t1,t2) ->
      let (ty1,c1, acX1) = collectR env t1 lastX in
      let (ty2,c2, acX2) = collectR env t2 acX1 in
      (ty2, List.concat [c1; c2; [(Tlist(ty1), ty2)] ], acX2)
    | Hd(t) ->
      let nX = newX lastX in
      let tX = Tvar nX in
      let (ty,c, acX) = collectR env t nX in
      (tX, List.concat [c; [(ty, Tlist tX)] ], acX)
    | Tl(t) ->
      let nX = newX lastX in
      let (ty,c,acX) = collectR env t nX in
      let tXlist =  Tlist (Tvar nX) in
      (tXlist, List.concat [c; [(ty, tXlist)] ], acX)
    | IsEmpty(t) ->
      let nX = newX lastX in
      let (ty,c,acX) = collectR env t nX in
      let tXlist =  Tlist (Tvar nX) in
      (Tbool, List.concat [c; [(ty, tXlist)] ], acX)
    | Raise -> (Tvar (newX lastX), [], lastX)
    | _ -> raise CollectFail

(* collect - função que coleta as equações de tipo do programa

    Uso: collect env program lastX , onde
      env : tEnv - ambiente de tipos do programa
      program : expr - programa a ser analisado
    retorna: (T, C), onde
      T : tipo - tipo do programa analisado
      C : tyEq list - lista de equações de tipo a serem resolvidas para determinar o tipo do programa
    pode gerar exceção UnifyFail.

    exemplos:
      collect [] (Binop(Sum, Ncte 5, Ncte 2)) deve retornar (Tint, [(Tint, Tint); (Tint, Tint)])
      collect [] App (Fn ("x", Binop (Sum, Var "x", Ncte 1)), Ncte 5) deve retornar
              (Tvar "1", [(Tvar "2", Tint); (Tint, Tint); (Tfn (Tvar "2", Tint), Tfn (Tint, Tvar "1"))])
*)
let collect (env : tEnv) (program : expr) : tipo * tyEq list =
    let (t,c,_) = collectR env program "0"
    in (t,c)



(*  occurs - função que verifica se t2 ocorre em t1

    Uso: occurs t1 t2 , onde:
      t1 : tipo
      t2 : tipo
    retorna: true se t2 ocorre em t1,
             false c.c.

    exemplos:
      occurs Tvar("x") Tvar("x") deve retornar true
      occurs Tint Tvar("x") deve retornar false
      occurs Tfn(Tint,Tvar("x")) Tvar("x") deve retornar true
*)
let rec occurs t1 t2 = match t1 with
    | t when (t1 = t2) -> true
    | Tfn(t3,t4) -> (occurs t3 t2) || (occurs t4 t2)
    | Tpair(t3,t4) -> (occurs t3 t2) || (occurs t4 t2)
    | Tlist(t3) -> (occurs t3 t2)
    | _ -> false


(* applysubs - função aplica substituições em um tipo ty

    uso: applysubs dom ty , onde
      dom : replacement list - substituições de variaveis encontradas pelo unify
      ty : tipo - tipo a ter variaveis substituidas
    retorna : o tipo com substituições aplicadas
              Tvar(X) se o tipo não puder ser substituido
*)
let rec applysubs (dom : replacement list) (ty : tipo) = match ty with
    | Tint -> Tint
    | Tbool -> Tbool
    | Tfn(t1, t2) -> Tfn(applysubs dom t1, applysubs dom t2)
    | Tpair(t1, t2) -> Tpair(applysubs dom t1, applysubs dom t2)
    | Tlist(t) -> Tlist(applysubs dom t)
    | Tvar(x) ->
      try lookup dom x
      with NotFound -> Tvar(x)


let rec applysubs' (dom : replacement list) (ty : tipo) (i : int)=
    let ty' = applysubs dom ty in
    match ty' with
    | Tvar(x) when (ty' = ty) -> ty'
    | Tvar(x) when (i < List.length dom)-> applysubs' dom ty' (i+1)
    | _ -> ty'


let rec applysubsInEqList (dom : replacement list) (tys : tyEq list) = match tys with
    | [] -> []
    | (t1,t2) :: tl -> (applysubs dom t1, applysubs dom t2) :: (applysubsInEqList dom tl)

(* unify - função que procura substituições de tipo em um conjunto de equações de tipos consts

   Uso: unify subs consts , onde
      subs : replacement list - lista de substituições de tipo encontradas até o momento
      consts : tyEq list - lista de equações de tipo a serem resolvidas para gerar as substituições de tipo
   retorna: a lista contendo as substituições de tipo encontradas.
   pode gerar exceção UnifyFail

   exemplos :
    unify [] [(Tint, Tint); (Tint, Tint)] deve retornar []
    unify [] (App (Fn ("x", Binop (Sum, Var "x", Ncte 1)), Ncte 5)) deve retornar [("2", Tint); ("2", Tint); ("1", Tint)]
*)
let rec unify (subs : replacement list) (consts : tyEq list) = match consts with
    | [] -> subs
    | (Tint, Tint) :: c -> unify subs c
    | (Tbool, Tbool) :: c -> unify subs c
    | (Tlist(t1), Tlist(t2) ) :: c -> unify subs ( (t1,t2) :: c )
    | (Tfn(t1,t2), Tfn(t3,t4) ) :: c -> unify subs ( (t1,t3) :: (t2,t4) :: c )
    | (Tpair(t1,t2), Tpair(t3,t4) ) :: c -> unify subs ( (t1,t3) :: (t2,t4) :: c )
    | (Tvar(x1), Tvar(x2)) :: c when (x1 = x2) -> unify subs c
    | (Tvar(x), t) :: c when (not (occurs t (Tvar x) )) ->
        unify (List.append subs [(x,t)]) (applysubsInEqList [(x,t)] c)
    | (t, Tvar(x)) :: c when (not (occurs t (Tvar x) )) ->
        unify (List.append subs [(x,t)]) (applysubsInEqList [(x,t)] c)
    | _ -> raise UnifyFail

(* unify step by step para fazer o debugg*)
let unify_step (subs : replacement list) (consts : tyEq list) = match consts with
    | [] -> (subs, [])
    | (Tint, Tint) :: c -> (subs, c)
    | (Tbool, Tbool) :: c -> (subs, c)
    | (Tlist(t1), Tlist(t2) ) :: c -> (subs, ( (t1,t2) :: c ))
    | (Tfn(t1,t2), Tfn(t3,t4) ) :: c -> (subs, ( (t1,t3) :: (t2,t4) :: c ) )
    | (Tpair(t1,t2), Tpair(t3,t4) ) :: c -> (subs, ( (t1,t3) :: (t2,t4) :: c ))
    | (Tvar(x1), Tvar(x2)) :: c when (x1 = x2) -> (subs, c)
    | (Tvar(x), t) :: c when (not (occurs t (Tvar x) )) ->
        ((List.append subs [(x,t)]), (applysubsInEqList [(x,t)] c))
    | (t, Tvar(x)) :: c when (not (occurs t (Tvar x) )) ->
        ((List.append subs [(x,t)]), (applysubsInEqList [(x,t)] c))
    | _ -> raise UnifyFail


(* typeinfer - função verifica o tipo de um programa

    uso: typeinfer env program , onde:
      env : tEnv - ambiente de tipos do programa
      program : expr - programa a ser analisado
    retorna : o tipo do programa
    pode gerar exceção TypeinferFail
*)
let typeinfer (env:tEnv) (program:expr) =
  let (t,c) = try collect env program
              with CollectFail -> raise TypeinferFail in
  let dom = try unify [] c
            with UnifyFail -> raise TypeinferFail in
  applysubs' dom t 0
