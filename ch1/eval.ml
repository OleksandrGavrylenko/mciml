open Straightline;;

let rec updatetbl tbl i v = match tbl with
    (item, value)::rest when item = i -> (i, v)::rest
    | x::rest -> x::(updatetbl rest i v)
    | [] -> [(i, v)]

let rec lookuptbl tbl i = match tbl with
    |(item ,value)::rest when item = i -> value
    | _::rest -> lookuptbl rest i
    | [] -> invalid_arg "Failed lookup"

let rec evalstm (tbl: (id * int) list) (s: stm) : (id * int) list = match s with
    Compound(st1, st2) -> evalstm (evalstm tbl st1) st2
    | Assign(i, v) ->
            let (t2, v2)  = evalexp tbl v in
            updatetbl t2 i v2
    | Print(exl) ->
            let f = (fun t ex ->
                let (t2, v) = evalexp t ex in
                (Printf.printf "%d\n" v; t2)) in
            List.fold_left f tbl exl
            (*let v, t2 = evalexp tbl ex in*)
            
and evalexp tbl ex = match ex with
    Id i -> (tbl, lookuptbl tbl i)
    | Num i -> tbl, i
    | Op(ex1, op, ex2) -> evalbin tbl op ex1 ex2
    | Eseq(st, ex) ->
            let t2 = evalstm tbl st in
            evalexp t2 ex

and evalbin tbl op ex1 ex2 =
    let (t1, v1) = evalexp tbl ex1 in
    let (t2, v2) = evalexp t1 ex2 in
    let v3 = (match op with
      Plus -> v1 + v2
    | Minus -> v1 - v2
    | Times -> v1 * v2
    | Div -> v1 / v2) in
    (t2, v3)


let printlist l = (Printf.printf "Final Values:\n"; List.map (fun((i, v)) -> Printf.printf "%s %d\n" i v) l);;

let _ = printlist (evalstm [] prog)

