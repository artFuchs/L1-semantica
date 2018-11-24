(* Necessita dos módilos Syntax, Env, Bigstep e Typeinfer *)
(* open Bigstep *)
#use "syntax.ml"
#use "env.ml"
#use "bigStep.ml"
#use "typeinfer.ml"

exception EvaluationFail

let evaluate expr =
    let t = try typeinfer [] expr
            with TypeinferFail -> raise EvaluationFail
    in match t with
      | Tvar(x) -> raise EvaluationFail
      | _ -> bs expr []

let value_string expr = match expr with
    | Vint n -> string_of_int n
    | Vbool b -> string_of_bool b
    | Vclos(_) -> "fn"
    | Vrclos(_) -> "rec fn"
    | Vnil -> "[]"
    | _ -> "asd"


(* Programa que busca o ultimo elemento de uma lista *)
(*
let rec last = fn l =>
    if isempty l
    then raise
    else
        let tail = tl l in
            if isempty tail
            then head l
            else last tail
   in last list
*)
let if_tail_empty = If( IsEmpty(Var "tail"),
                        Hd(Var "l"),
                        App(Var "last",Var "tail")
                      )
let lastElse = Let( "tail",
                    Tl(Var "l"),
                    if_tail_empty
                  )
let last = Fn( "list",
               Lrec("last",
                    "l",
                    If(IsEmpty(Var "l"), Raise, lastElse),
                    App(Var "last", Var "list")
               )
          )

let myList = Cons(Ncte 1, Cons(Ncte 2, Nil))
let nilMyList = Tl(Tl myList)
