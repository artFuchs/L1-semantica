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

exception NotFound

let rec lookup (environment : env) (x : variable) = match environment with
  | [] -> raise NotFound
  | (var, v) :: tl when (var = x) -> v
  | hd :: tl -> lookup tl x

let rec updateEnv (environment : env) (x : variable) (v : value) = match environment with
  | [] -> [(x,v)]
  | hd :: tl -> hd :: updateEnv tl x v
