#use "bigStep.ml"

let evalTest term env answer =
    if bs term env = answer
    then true
    else false

let rec evaluateTests tests = match tests with
    |[] -> []
    |(term, env, answer) :: tl -> (evalTest term env answer) :: evaluateTests tl

(* tests with operations *)
let tSum1 = (Binop(Sum, Ncte 1, Ncte 1), [], Vint 2 )
let tSub1 = (Binop(Sub, Ncte 5, Ncte 1), [], Vint 4 )
let tMult1 = (Binop(Mult, Ncte 4, Ncte 2), [], Vint 8 )
let tDiv1 = (Binop(Div, Ncte 4, Ncte 2), [], Vint 2 )

let tAndTT = ( Binop(And, Bcte true, Bcte true), [], Vbool true )
let tAndTF = ( Binop(And, Bcte true, Bcte false), [], Vbool false )
let tAndFF = ( Binop(And, Bcte false, Bcte false), [], Vbool false )

let tOrTT = ( Binop(Or, Bcte true, Bcte true), [], Vbool true )
let tOrTF = ( Binop(Or, Bcte true, Bcte false), [], Vbool true )
let tOrFF = ( Binop(Or, Bcte false, Bcte false), [], Vbool false )

let tEqT = ( Binop(Eq, Ncte 5, Ncte 5), [], Vbool true )
let tEqF = ( Binop(Eq, Ncte 10, Ncte 5), [], Vbool false )
let tDfT = ( Binop(Df, Ncte 10, Ncte 5), [], Vbool true )
let tDfF = ( Binop(Df, Ncte 5, Ncte 5), [], Vbool false )

let tLtF = ( Binop(Lt, Ncte 5, Ncte 5), [], Vbool false )
let tLtT = ( Binop(Lt, Ncte 5, Ncte 10), [], Vbool true )
let tLeT1 = ( Binop(Le, Ncte 5, Ncte 10), [], Vbool true )
let tLeT2 = ( Binop(Le, Ncte 5, Ncte 5), [], Vbool true )
let tLeF = ( Binop(Le, Ncte 6, Ncte 5), [], Vbool false )

let tGtT = ( Binop(Gt, Ncte 6, Ncte 5), [], Vbool true )
let tGtF = ( Binop(Gt, Ncte 5, Ncte 5), [], Vbool false )
let tGeF = ( Binop(Ge, Ncte 5, Ncte 6), [], Vbool false )
let tGeT1 = ( Binop(Ge, Ncte 5, Ncte 5), [], Vbool true )
let tGeT2 = ( Binop(Ge, Ncte 6, Ncte 5), [], Vbool true )

let tNotT = ( Unop(Not, Bcte(true)), [], Vbool false )
let tNotF = ( Unop(Not, Bcte(false)), [], Vbool true )

let opTests = [ tSum1;
                tSub1;
                tMult1;
                tDiv1;
                tAndTT;
                tAndTF;
                tAndFF;
                tOrTT;
                tOrTF;
                tOrFF;
                tEqT;
                tEqF;
                tDfT;
                tDfF;
                tLtF;
                tLtT;
                tLeT1;
                tLeT2;
                tLeF;
                tGtT;
                tGtF;
                tGeF;
                tGeT1;
                tGeT2;
                tNotT;
                tNotF;
                ]
