open Straightline;;

let listmax l = match l with
    [] -> invalid_arg "empty list"
    | x::xs -> List.fold_left max x xs

let rec maxargs : stm -> int  = fun s ->
    match s with
    Compound (st1, st2) -> max (maxargs st1) (maxargs st2)
    | Assign (_, ex) -> maxargex ex
    | Print l -> max (List.length l) (listmax (List.map maxargex l))

   and maxargex ex = match ex with
        Op (ex1, _, ex2) -> max (maxargex ex1) (maxargex ex2)
        | Eseq (st, ex) -> max (maxargs st) (maxargex ex)
        | _ -> 0
    
let main () = Printf.printf "%d\n" (maxargs prog);;

main ();;
