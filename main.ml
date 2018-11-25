(* Necessita dos módilos Syntax, Env, Bigstep e Typeinfer *)
(* open Bigstep *)
#use "syntax.ml"
#use "env.ml"
#use "bigStep.ml"
#use "typeinfer.ml"

exception EvaluationFail
exception ValueStringFail

let evaluate expr =
    let t = try typeinfer [] expr
            with TypeinferFail -> raise EvaluationFail
    in match t with
      | Tvar(x) -> raise EvaluationFail
      | _ -> bs expr []

let rec value_string expr = match expr with
    | Vint n -> string_of_int n
    | Vbool b -> string_of_bool b
    | Vclos(_) -> "fn"
    | Vrclos(_) -> "rec fn"
    | Vnil -> "Nil"
    | Vlist([Vnil]) -> "Nil"
    | Vlist(x::xs) -> (value_string x) ^ "::" ^ (value_string (Vlist xs))
    | Vpair(v1,v2) -> "( " ^ (value_string v1) ^ "," ^ (value_string v2) ^ ")"
    | Vraise -> "raise"
    | _ -> raise ValueStringFail


(* Programa que busca o ultimo elemento de uma lista *)
let lastElse = Let( "tail",
                    Tl(Var "l"),
                    If( IsEmpty(Var "tail"),
                        Hd(Var "l"),
                        App(Var "last",Var "tail")
                      )
                  )
let last = Fn( "list",
               Lrec("last",
                    "l",
                    If(IsEmpty(Var "l"), Raise, lastElse),
                    App(Var "last", Var "list")
               )
          )

let last3static = Lrec("last",
                 "l",
                 If(IsEmpty(Var "l"), Raise, lastElse),
                 App(Var "last",
                     Cons(Ncte 1, Cons(Ncte 2, Cons(Ncte 3, Nil)))
                 )
            )
(* Programa que verifica se o primeiro de uma lista é o ultimo,
    retornando (true, elem) se for e
 um elemento é o ultimo da lista, retornando()*)
let isLast = Fn( "l",
                  If ( IsEmpty(Tl(Var "l")),
                      Pair(Bcte(true), Hd(Var "l")),
                      Pair(Bcte(false),Hd(Var "l"))
                    )
                )

let list4 = Cons(Ncte 1, Cons(Ncte 2, Cons(Ncte 3, Cons(Ncte 4, Nil))))
let list1 = Cons(Ncte 1, Nil)

let appLast4 = App(last, list4)
let appLast1 = App(last, list1)
