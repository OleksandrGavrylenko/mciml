open Core.Std

type symbol = string * int [@@deriving show]
let nextsym = ref 0
let dummy = ("Dummy", -1)
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

let reverseFind intval : string option = Hashtbl.fold table ~init:None ~f:(fun ~key:k ~data:v acc -> if v = intval then Some k else acc)

type 'a table = 'a Int.Map.t
let empty = Int.Map.empty

let showKeys map =
    print_endline "Keys:";
    List.map (Map.keys map) (fun x ->
        let Some v = reverseFind x in
        Format.printf "%s (%d)@." v x);
    ()

let enter t (s, n) a =
    showKeys t;
    print_endline ("Adding symbol: " ^ s);
    Map.add t ~key:n ~data:a

let find t (s, n) =
    showKeys t;
    print_endline ("Finding symbol: " ^ s);
    Map.find t n

