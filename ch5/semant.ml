module Env : sig
    type access
    type ty = Types.ty
    type enventry = VarEntry of ty | FunEntry of (ty list * ty)
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end = struct
    type ty = Types.ty
    type access = unit
    type enventry = VarEntry of ty | FunEntry of (ty list * ty)
    let base_tenv = Symbol.empty
    let base_venv = Symbol.empty
end

module Translate = struct type exp = unit end

module A = Absyn

type expty = {exp: Translate.exp; ty: Types.ty}

(*
transVar: venv * tenv * Absyn.var -> expty
transExp: venv * tenv * Absyn.exp -> expty
transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
transTy: tenv * Absyn.ty -> Types.ty
*)

let checkInt {exp=_; ty=ty} pos = assert (ty == Types.Int)

let rec get_record_type rcds sym = match rcds with
    | (sym2, ty)::rest -> if sym == sym2 then {exp=(); ty=ty} else get_record_type rest sym
    | [] -> raise Types.TypeNotFound

let transExp venv tenv =
    let rec trexp = function
        | A.VarExp v -> trvar v
        | A.NilExp -> {exp=(); ty=Types.Nil}
        | A.IntExp _ -> {exp=(); ty=Types.Int}
        | A.StringExp _ -> {exp=(); ty=Types.String}
        (*| A.CallExp(sym, exps, pos) ->*)
        | A.OpExp(left, op, right, pos) -> begin
                checkInt (trexp left) pos;
                checkInt (trexp right) pos;
                {exp=(); ty=Types.Int}
            end

    and trvar = function
        | A.SimpleVar(id, pos) -> begin
            match Symbol.find venv id with
            | Some (Env.VarEntry ty) -> {exp=(); ty=Types.actual_ty ty}
            | None -> raise Types.TypeNotFound
          end
        | A.FieldVar (var, sym, pos) -> 
            let {exp=_; ty=Types.Record (rcds, _)} = trvar var in
            get_record_type rcds sym
        | A.SubscriptVar (var, exp, pos) -> {exp=(); ty=Types.Int}
    in trexp

(* 1 + 2 *)
let testexp = A.OpExp(A.IntExp 1, A.PlusOp, A.IntExp 2, Lexing.dummy_pos)

let () = transExp Env.base_venv Env.base_tenv testexp; print_endline "Didn't break!"

(*
and exp = VarExp of var
            | NilExp
            | IntExp of int
            | StringExp of string * pos
            | CallExp of symbol * exp list * pos
            | OpExp of exp * oper * exp * pos
            | RecordExp of (symbol * exp * pos) list * symbol * pos
            | SeqExp of (exp * pos) list
            | AssignExp of var * exp * pos
            | IfExp of exp * exp * exp option * pos
            | WhileExp of exp * exp * pos
            | ForExp of symbol * bool ref * exp * exp * exp * pos
            | BreakExp of pos
            | LetExp of dec list * exp * pos
            | ArrayExp of symbol * exp * exp * pos
            [@@deriving show]
*)

