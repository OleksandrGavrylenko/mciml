open Core.Std

(* I think the point of this module is simply to define a hashtable that
 * can hold symbols *)

module Symbol : sig
  type symbol
  val sym: string -> symbol
  val name : symbol -> string
  (*
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
  *)
end = struct
  type symbol = string * int
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

  (*
  type 'a table= 'a Table.table
  let empty = Table.empty
  let enter = Table.enter
  let look = Table.look
  *)
end
