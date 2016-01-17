open Core.Std

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

    let base_tenv =
      let intsym = Symbol.sym "int"
      and strsym = Symbol.sym "string"
      and m = Symbol.empty in
      let m = Symbol.enter m intsym Types.Int in
      Symbol.enter m strsym Types.String

    let base_venv = Symbol.empty
end

module Translate = struct type exp = unit end

module A = Absyn

type expty = {exp: Translate.exp; ty: Types.ty}

(*
transVar: venv * tenv * Absyn.var -> expty
transExp: venv * tenv * Absyn.exp -> expty
transDec: venv * tenv * Absyn.dec -> (venv, tenv)
transTy: tenv * Absyn.ty -> Types.ty
*)

let checkInt {exp=_; ty=ty} pos = assert (ty = Types.Int)
let assertEq v1 v2 = assert (v1 = v2)
let unwrap {ty;_} = ty

let getSym env sym = match Symbol.find env sym with
    | Some r -> r
    | None -> Symbol.showKeys env; raise (Types.TypeNotFound sym)

let rec get_record_type rcds sym = match rcds with
    | (sym2, ty)::rest -> if sym = sym2 then {exp=(); ty=ty} else get_record_type rest sym
    | [] -> raise (Types.TypeNotFound sym)

let transTy tenv ty =
  let mkRecord fields =
      print_endline "Making fields...";
      let mkrec ({name; typ; _}: A.field) =
          print_endline ("Making field: " ^ Symbol.name name);
          (name, getSym tenv typ) in
      let r = Types.Record (List.map fields mkrec, ref ()) in
      print_endline "Done";
      r
  in
  match ty with
    | A.NameTy (typnm, pos) -> getSym tenv typnm
    | A.RecordTy fields -> mkRecord fields
    | A.ArrayTy (typnm, pos) -> Types.Array (getSym tenv typnm, ref ()) 

let rec transExp venv tenv =
    let rec trexp = function
        | A.VarExp v -> trvar v
        | A.NilExp -> {exp=(); ty=Types.Nil}
        | A.IntExp _ -> {exp=(); ty=Types.Int}
        | A.StringExp _ -> {exp=(); ty=Types.String}
        | A.RecordExp (rcds, sym, pos) -> 
            let ty = getSym tenv sym in
            let Types.Record (flds, _) = ty in
            check_record_types flds rcds; {exp=(); ty=ty}
        | A.CallExp(sym, exps, pos) ->
            let Env.FunEntry (argtys, rtnty) = getSym venv sym in
            check_func_call argtys exps; {exp=(); ty=rtnty}
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
        | A.LetExp (decs, exp, pos) -> 
            let (venvupd, tenvupd) = transDecs venv tenv decs in
            transExp venvupd tenvupd exp

    and check_record_types wanted found = match (wanted, found) with
        | ((sym1, ty1)::wntd, (sym2, exp, pos)::fnd) -> 
            print_endline "assert record syms";
            print_endline (Symbol.name sym1);
            print_endline (Symbol.name sym2);
            assertEq (Symbol.name sym1) (Symbol.name sym2);
            print_endline "assert record types";
            assertEq ty1 (unwrap (trexp exp));
            check_record_types wntd fnd
        | ((s,t)::xs, []) -> raise (Types.TypeNotFound s)
        | ([], (s,_,_)::xs) -> raise (Types.TypeNotFound s)
        | ([], []) -> ()

    and check_func_call argtys exps = match (argtys, exps) with
        | (ty::args, e::exps) -> assertEq ty (unwrap (trexp e)); check_func_call args exps
        | (x::xs, []) -> raise (Types.TypeNotFound Symbol.dummy)
        | ([], x::xs) -> raise (Types.TypeNotFound Symbol.dummy)
        | ([], []) -> ()

    and check_assignment var exp =
        let ty1 = unwrap (trvar var)
        and ty2 = unwrap (trexp exp) in
        assertEq ty1 ty2; {exp=(); ty=Types.Nil}

    and trvar = function
        | A.SimpleVar(id, pos) ->
            let Env.VarEntry ty = getSym venv id in
            {exp=(); ty=Types.actual_ty ty}
        | A.FieldVar (var, sym, pos) -> 
            let {exp=_; ty=Types.Record (rcds, _)} = trvar var in
            get_record_type rcds sym
        | A.SubscriptVar (var, exp, pos) ->
            let {exp=_; ty=Types.Array (ty, _)} = trvar var
            in checkInt (trexp exp); {exp=(); ty=ty}
    in trexp

and transDecs venv tenv decs = List.fold_left decs ~init:(venv, tenv) ~f:update_env

and update_env (venv, tenv) dec =

    let funcdec {A.name; params; result; body; pos} =
      let resty = match result with
        | Some (name, pos) -> getSym tenv name
        | None -> Types.Nil 
      and transparam ({A.name; typ; _}: A.field) = (name, getSym tenv name)
      and getTy (_, ty) = ty in
      let params' = List.map params transparam in
      let venv'  = Symbol.enter venv name (Env.FunEntry (List.map params' getTy, resty)) in
      let enterparam venv (name, ty) = Symbol.enter venv name (Env.VarEntry ty) in
      let venv'' = List.fold_left params' ~init: venv ~f: enterparam
      in transExp venv'' tenv body; (venv', tenv)

    in match dec with
      | A.FunctionDec fundec -> funcdec fundec
      | A.VarDec {name; typ=tyopt; init=exp; _} ->
        let {exp; ty} = transExp venv tenv exp in
        let () = match tyopt with 
          | Some (sym, pos) -> assertEq (getSym tenv sym) ty
          | None -> () in
        (Symbol.enter venv name (Env.VarEntry ty), tenv)
      | A.TypeDec (sym, ty, pos) -> (venv, Symbol.enter tenv sym (transTy tenv ty))


let testast = Tigparse.parse_string "
   let
     type arr = array of int
     type r = {first: int, rest: string}
     var s: string := \"Hello\"
     var r1: r := r{first=10, rest=\"Hiya\"}
     var i: int := 10
   in
     s;
     i := i + i + 2 - 3 / 4 * 5;
     r1.first;
     r1.rest
   end"

let () =
    print_endline "Testing AST:";
    Tigparse.print_ast testast;
    try
      let _ = transExp Env.base_venv Env.base_tenv testast in ()
    with
      | Types.TypeNotFound sym ->
        print_endline ("Error - unknown symbol: " ^ Symbol.name sym)

(*
            | IfExp of exp * exp * exp option * pos
            | WhileExp of exp * exp * pos
            | ForExp of symbol * bool ref * exp * exp * exp * pos
            | BreakExp of pos
            | ArrayExp of symbol * exp * exp * pos
*)
