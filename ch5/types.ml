type unique = unit ref

exception TypeNotFound

type ty = 
        Record of (Symbol.symbol * ty) list * unique
        | Nil
        | Int
        | String
        | Array of ty * unique
        | Name of Symbol.symbol * ty option ref
	    | Unit

let actual_ty ty = match ty with 
        | Name (_, optty) ->
          begin
            match !optty with
            | Some ty -> ty
            | None -> raise TypeNotFound
          end
        | ty -> ty

