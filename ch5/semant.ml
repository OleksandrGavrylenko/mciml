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


let assertTyEq ty1 ty2 pos =
  if (ty1 <> ty2) then raise (Types.UnexpectedType (ty1, ty2, pos))

let checkInt {exp=_; ty=ty} pos = assertTyEq ty Types.Int pos

let assertEq v1 v2 = assert (v1 = v2)

let unwrap {ty;_} = ty
let wrap ty = {exp=(); ty=ty}

let getSym env sym = match Symbol.find env sym with
    | Some r -> r
    | None -> Symbol.showKeys env; raise (Types.TypeNotFound (sym, Lexing.dummy_pos))

let rec get_record_type rcds sym = match rcds with
    | (sym2, ty)::rest -> if sym = sym2 then wrap ty else get_record_type rest sym
    | [] -> raise (Types.TypeNotFound (sym, Lexing.dummy_pos))

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
        | A.NilExp -> wrap Types.Nil
        | A.UnitExp -> wrap Types.Unit
        | A.IntExp _ -> wrap Types.Int
        | A.StringExp _ -> wrap Types.String
        | A.RecordExp (rcds, sym, pos) -> 
            let ty = getSym tenv sym in
            let Types.Record (flds, _) = ty in
            check_record_types flds rcds; wrap ty
        | A.CallExp(sym, exps, pos) ->
            let Env.FunEntry (argtys, rtnty) = getSym venv sym in
            check_func_call argtys exps; wrap rtnty
        | A.SeqExp exprs -> begin
            match exprs with
            | (e, _)::exps -> trexp e; trexp (A.SeqExp exps)
            | [(e, _)] -> trexp e
            | [] -> wrap Types.Unit
          end
        | A.OpExp(left, op, right, pos) -> begin
                checkInt (trexp left) pos;
                checkInt (trexp right) pos;
                wrap Types.Int
            end
        | A.AssignExp(var, exp, pos) -> check_assignment var exp
        | A.LetExp (decs, exp, pos) -> 
            let (venvupd, tenvupd) = transDecs venv tenv decs in
            transExp venvupd tenvupd exp
        | A.IfExp (ifexp, thenexp, elseexpopt, pos) ->
          let () = checkInt (trexp ifexp) pos in
          let thenty = unwrap (trexp thenexp) in
          let () = match elseexpopt with
            | Some elseexp -> assertTyEq (unwrap (trexp elseexp)) thenty pos
            | None -> assertTyEq thenty Types.Unit pos in
          wrap thenty
        | A.WhileExp (whileexp, loopexp, pos) ->
          checkInt (trexp whileexp) pos;
          assertTyEq (unwrap (trexp loopexp)) Types.Unit pos;
          wrap Types.Unit
        | A.ForExp(sym, _, assignexp, toexp, loopexp, pos) ->
          checkInt (trexp assignexp) pos;
          checkInt (trexp toexp) pos;
          assertTyEq (unwrap (trexp loopexp)) Types.Unit pos;
          wrap Types.Unit
        | A.BreakExp (pos) -> wrap Types.Unit
        | A.ArrayExp(sym, ctexp, initexp, pos) ->
          checkInt (trexp ctexp) pos;
          checkInt (trexp initexp) pos;
          wrap (getSym tenv sym)

    and check_record_types wanted found = match (wanted, found) with
        | ((sym1, ty1)::wntd, (sym2, exp, pos)::fnd) -> 
            print_endline (Symbol.name sym1);
            print_endline (Symbol.name sym2);
            assertEq (Symbol.name sym1) (Symbol.name sym2);
            assertTyEq (unwrap (trexp exp)) ty1 pos;
            check_record_types wntd fnd
        | ((s,t)::xs, []) -> raise (Types.TypeNotFound (s, Lexing.dummy_pos))
        | ([], (s,_,_)::xs) -> raise (Types.TypeNotFound (s, Lexing.dummy_pos))
        | ([], []) -> ()

    and check_func_call argtys exps = match (argtys, exps) with
        | (ty::args, e::exps) ->
          assertTyEq (unwrap (trexp e)) ty;
          check_func_call args exps
        | (x::xs, []) -> raise (Types.TypeNotFound (Symbol.dummy, Lexing.dummy_pos))
        | ([], x::xs) -> raise (Types.TypeNotFound (Symbol.dummy, Lexing.dummy_pos))
        | ([], []) -> ()

    and check_assignment var exp =
        let ty1 = unwrap (trvar var)
        and ty2 = unwrap (trexp exp) in
        assertTyEq ty2 ty1; wrap Types.Unit

    and trvar = function
        | A.SimpleVar(id, pos) ->
            let Env.VarEntry ty = getSym venv id in
            wrap (Types.actual_ty ty)
        | A.FieldVar (var, sym, pos) -> 
            let {exp=_; ty=Types.Record (rcds, _)} = trvar var in
            get_record_type rcds sym
        | A.SubscriptVar (var, exp, pos) ->
            let {exp=_; ty=Types.Array (ty, _)} = trvar var
            in checkInt (trexp exp); wrap ty
    in trexp

and transDec (venv, tenv) dec =
  let funcdec {A.name; params; result; body; pos} =
    let resty = match result with
      | Some (name, pos) -> getSym tenv name
      | None -> Types.Unit 
    and transparam ({A.name; typ; _}: A.field) = (name, getSym tenv typ)
    and getTy (_, ty) = ty in
    let params' = List.map params transparam in
    let venv' = Symbol.enter venv name (Env.FunEntry (List.map params' getTy, resty))
    and enterparam env (name, ty) = Symbol.enter env name (Env.VarEntry ty) in
    let venv'' = List.fold_left params' ~init: venv' ~f: enterparam in
    print_endline "Here mdear";
    print_endline (Types.show_ty resty);
    assertTyEq (unwrap (transExp venv'' tenv body)) resty;
    (venv', tenv)

  in match dec with
    | A.FunctionDec fundec -> funcdec fundec
    | A.VarDec {name; typ=tyopt; init=exp; _} ->
      let {exp; ty} = transExp venv tenv exp in
      let () = match tyopt with 
        | Some (sym, pos) -> assertTyEq (getSym tenv sym) ty pos
        | None -> () in
      (Symbol.enter venv name (Env.VarEntry ty), tenv)
    | A.TypeDec (sym, ty, pos) -> (venv, Symbol.enter tenv sym (transTy tenv ty))

and transDecs venv tenv decs = List.fold_left decs ~init:(venv, tenv) ~f:transDec




let testast = Tigparse.parse_string "
   /* premably comment blah */
   let
     type intArr = array of int
     type r = {first: int, rest: string}
     var s: string := \"Hello\"
     var r1: r := r{first=10, rest=\"Hiya\"}
     var i: int := 10
     var j: string := 10
     var arr: intArr := intArr[j + j] of 0

     function myfunc(k: int) : int =
       let
         var l: int := 10
       in
       end
   in
     s;
     i := i + i + 2 - 3 / 4 * 5;
     r1.first;
     r1.rest;
     while i = 10 do i := i + 10;
     if i > 0 then i := i * i;
     if i > 0 then i + 1 else i - 1;
     for k := 10 to 20 do i := i + 10;
     j := \"OIoi\";
     arr[10] := j;
     j
   end"

let testast2 = Tigparse.parse_string "
   let
     function myfunc() =
       let
         var l: int := 10
       in
         \"heeeeello\"
       end
   in
   end"

let () =
    print_endline "Testing AST:";
    Tigparse.print_ast testast;
    try
      let _ = transExp Env.base_venv Env.base_tenv testast in ()
    with
      | Types.TypeNotFound (sym, pos) ->
        Format.printf "Error at %s - unknown symbol: %s" (A.show_pos pos) (Symbol.name sym)
      | Types.UnexpectedType (found, expect, pos) ->
        Format.printf "Error at %s - expected type %s but found type %s@." (A.show_pos pos) (Types.show_ty found) (Types.show_ty expect) 

