open Core.Std

type symbol = string * int [@@deriving show]
let nextsym = ref 0
let sizeHint = 128
let table = String.Table.create ()

let sym name = match Hashtbl.find table name with
    | Some i -> (name, i)
    | None -> begin
                let i = !nextsym in
                nextsym := i+1;
                let _ = Hashtbl.add table name i in
                (name, i)
              end

let name (s, _) = s
