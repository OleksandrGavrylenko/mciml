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
let assertEq v1 v2 = assert (v1 == v2)
let unwrap {exp=_; ty=ty} = ty

let rec get_record_type rcds sym = match rcds with
    | (sym2, ty)::rest -> if sym == sym2 then {exp=(); ty=ty} else get_record_type rest sym
    | [] -> raise Types.TypeNotFound

let transExp venv tenv =
    let rec trexp = function
        | A.VarExp v -> trvar v
        | A.NilExp -> {exp=(); ty=Types.Nil}
        | A.IntExp _ -> {exp=(); ty=Types.Int}
        | A.StringExp _ -> {exp=(); ty=Types.String}
        | A.RecordExp (rcds, sym, pos) -> begin
            match Symbol.find tenv sym with
            | Some ty -> let Types.Record (flds, _) = ty in
                check_record_types flds rcds; {exp=(); ty=ty}
            | None -> raise Types.TypeNotFound;
          end
        (*| A.CallExp(sym, exps, pos) ->*)
        | A.SeqExp exprs -> begin
            match exprs with
            | (e, _)::exps -> trexp e; trexp (A.SeqExp exps)
            | [(e, _)] -> trexp e
            | [] -> {exp=(); ty=Types.Nil}
          end
        | A.OpExp(left, op, right, pos) -> begin
                checkInt (trexp left) pos;
                checkInt (trexp right) pos;
                {exp=(); ty=Types.Int}
            end
        | A.AssignExp(var, exp, pos) -> check_assignment var exp

    and check_record_types wanted found = match (wanted, found) with
        | ((sym1, ty1)::wntd, (sym2, exp, pos)::fnd) -> 
            assertEq sym1 sym2;
            assertEq ty1 (unwrap (trexp exp));
            check_record_types wntd fnd
        | (x::xs, []) -> raise Types.TypeNotFound
        | ([], x::xs) -> raise Types.TypeNotFound
        | ([], []) -> ()

    and check_assignment var exp =
        let ty1 = unwrap (trvar var)
        and ty2 = unwrap (trexp exp) in
        assertEq ty1 ty2; {exp=(); ty=Types.Nil}

    and trvar = function
        | A.SimpleVar(id, pos) -> begin
            match Symbol.find venv id with
            | Some (Env.VarEntry ty) -> {exp=(); ty=Types.actual_ty ty}
            | None -> raise Types.TypeNotFound
          end
        | A.FieldVar (var, sym, pos) -> 
            let {exp=_; ty=Types.Record (rcds, _)} = trvar var in
            get_record_type rcds sym
        | A.SubscriptVar (var, exp, pos) ->
            let {exp=_; ty=Types.Array (ty, _)} = trvar var
            in checkInt (trexp exp); {exp=(); ty=ty}
    in trexp

let testast = print_endline "Parsing";
              Tigparse.parse_file "../testprogs/simplemaths.tig"

let () =
    print_endline "Testing AST";
    transExp Env.base_venv Env.base_tenv testast;
    print_endline "Didn't break!"

(*
            | CallExp of symbol * exp list * pos
            | AssignExp of var * exp * pos
            | IfExp of exp * exp * exp option * pos
            | WhileExp of exp * exp * pos
            | ForExp of symbol * bool ref * exp * exp * exp * pos
            | BreakExp of pos
            | LetExp of dec list * exp * pos
            | ArrayExp of symbol * exp * exp * pos
*)

