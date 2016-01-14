type unique = unit ref

type ty = 
        Record of (Symbol.symbol * ty) list * unique
        | Nil
        | Int
        | String
        | Array of ty * unique
        | Name of Symbol.symbol * ty option ref
	    | Unit
