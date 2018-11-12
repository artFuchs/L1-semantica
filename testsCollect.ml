#use "typeinfer.ml"

let evalCollect term env answer =
    if collect env term = answer
    then true
    else false

let rec evaluateCTests tests = match tests with
    |[] -> []
    |(term, env, answer) :: tl -> (evalCollect term env answer) :: evaluateCTests tl

let ctNot = (Unop(Not, Bcte true), [], (Tbool, [(Tbool, Tbool)]))
let ctSum = (Binop(Sum, Ncte 1, Ncte 1), [], (Tint, [(Tint, Tint); (Tint, Tint)]))
let ctAnd = (Binop(And, Bcte true, Bcte false), [], (Tbool, [(Tbool, Tbool); (Tbool, Tbool)]))
let ctEq = (Binop(Eq, Ncte 4, Ncte 5), [], (Tbool, [(Tint, Tint); (Tint, Tint)]))

let ctList = [ ctNot;
               ctSum;
               ctAnd;
               ctEq ]

let ctAns = evaluateCTests ctList
