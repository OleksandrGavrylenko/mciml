type unique = unit ref [@@deriving show]

type ty = 
  | Record of (Symbol.symbol * ty) list * unique [@printer fun fmt rcd -> fprintf fmt "Record"]
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.symbol * ty option ref
  | Unit
  [@@deriving show]

exception TypeNotFound of (Symbol.symbol * Lexing.position)
exception UnexpectedType of (ty * ty * Lexing.position)
exception UnexpectedSymbol of (Symbol.symbol * Symbol.symbol * Lexing.position)

let actual_ty ty = match ty with 
  | Name (sym, optty) ->
    begin
      match !optty with
      | Some ty -> ty
      | None -> raise (TypeNotFound (sym, Lexing.dummy_pos))
    end
  | ty -> ty

