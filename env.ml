(* Usa o modulo syntax.ml *)
#use "syntax.ml"

type value = Vint of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | Vnil
           | Vlist of value list
           | Vpair of value * value
           | Vraise
    and
       env = (variable * value) list

type tipo = Tvar
          | Tint
          | Tbool
          | Tfunc of tipo * tipo
          | Tlist of tipo list
          | Tpair of tipo * tipo
   and
      tEnv = (variable * tipo) list

exception NotFound

let rec lookup environment (x : variable) = match environment with
  | [] -> raise NotFound
  | (var, v) :: tl when (var = x) -> v
  | hd :: tl -> lookup tl x

let rec updateEnv (environment : env) (x : variable) (v : value) = match environment with
  | [] -> [(x,v)]
  | hd :: tl -> hd :: updateEnv tl x v

let rec updateTEnv (environment : tEnv) (x : variable) (t : tipo) = match environment with
  | [] -> [(x,t)]
  | hd :: tl -> hd :: updateTEnv tl x t
