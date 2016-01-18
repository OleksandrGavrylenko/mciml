type unique = unit ref [@@deriving show]

type ty = 
  | Record of (Symbol.symbol * ty) list * unique
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.symbol * ty option ref
  | Unit
  [@@deriving show]

exception TypeNotFound of (Symbol.symbol * Lexing.position)
exception UnexpectedType of (ty * ty * Lexing.position)

let actual_ty ty = match ty with 
  | Name (_, optty) ->
    begin
      match !optty with
      | Some ty -> ty
      | None -> raise (TypeNotFound (Symbol.dummy, Lexing.dummy_pos))
    end
  | ty -> ty

