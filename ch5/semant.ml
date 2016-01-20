open Core.Std

module Env : sig
    type access
    type ty = Types.ty
    type enventry = VarEntry of ty | FunEntry of ((Symbol.symbol * ty) list * ty)
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end = struct
    type ty = Types.ty
    type access = unit
    type enventry = VarEntry of ty | FunEntry of ((Symbol.symbol * ty) list * ty)

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
module T = Types

let p = print_endline

exception GenericErr of (string * Lexing.position)

type expty = {exp: Translate.exp; ty: T.ty}

let checkSymEq found expect pos = 
  if (found <> expect) then raise (T.UnexpectedSymbol (found, expect, pos))

let checkTyEq found expect pos =
  let found2 = T.actual_ty found in
  let expect2 = T.actual_ty expect in
  match (found2, expect2) with
    | (T.Record (_, r1), T.Record (_, r2)) -> if r1 <> r2 then
        raise (T.UnexpectedType (found, expect, pos))
    | (t1, t2) -> if (found2 <> expect2) then
      raise (T.UnexpectedType (found, expect, pos))

let checkInt {exp=_; ty=ty} pos = checkTyEq ty T.Int pos

let rec checkTyEqOrNil found expect pos =
  match (T.actual_ty found, T.actual_ty expect) with
    | (T.Nil, T.Record _) -> ()
    | _ -> checkTyEq found expect pos

let rec checkTyEqOrNilSym left right pos =
  match (left, right) with
    | (T.Nil, T.Record _) -> ()
    | (T.Record _, T.Nil ) -> ()
    | (T.Nil, T.Nil) -> raise (GenericErr ("Cannot compare nil to nil\n", pos))
    | _ -> checkTyEq left right pos

let unwrap {ty;_} = ty
let wrap ty = {exp=(); ty=ty}

let getSym env sym pos = match Symbol.find env sym with
  | Some r -> r
  | None -> raise (T.TypeNotFound (sym, pos))

let rec get_record_type rcds sym =
    match rcds with
  | (sym2, ty)::rest -> if sym = sym2 then wrap ty else get_record_type rest sym
  | [] -> raise (T.TypeNotFound (sym, Lexing.dummy_pos))

let transTy tenv ty =
  let mkRecord fields =
    let mkrec ({fldname; fldtyp; fldpos; _}: A.field) = 
      (fldname, getSym tenv fldtyp fldpos) in
    T.Record (List.map fields mkrec, ref ()) in
  match ty with
    | A.NameTy (typnm, pos) -> getSym tenv typnm pos
    | A.RecordTy fields -> mkRecord fields
    | A.ArrayTy (typnm, pos) -> T.Array (getSym tenv typnm pos, ref ()) 

let rec transExp venv tenv =
    let rec trexp = function
        | A.VarExp v -> trvar v
        | A.NilExp -> wrap T.Nil
        | A.UnitExp -> wrap T.Unit
        | A.IntExp _ -> wrap T.Int
        | A.StringExp _ -> wrap T.String
        | A.RecordExp (rcds, sym, pos) -> 
          let ty = T.actual_ty (getSym tenv sym pos) in
          let T.Record (flds, _) = ty in
          let ()  = check_record_types rcds flds pos in
          wrap ty
        | A.CallExp(sym, exps, pos) ->
            let Env.FunEntry (argtys, rtnty) = getSym venv sym pos in
            let () = check_func_call argtys exps pos in
            wrap rtnty
        | A.SeqExp exprs ->
            List.fold_left exprs ~init: (wrap T.Unit) ~f: (fun ty (expr, pos) -> trexp expr)
        | A.OpExp(left, op, right, pos) -> begin
                checkTyEqOrNilSym (unwrap (trexp right)) (unwrap (trexp left)) pos;
                wrap T.Int
            end
        | A.AssignExp(var, exp, pos) -> check_assignment var exp pos
        | A.LetExp (decs, exp, pos) -> 
            let (venvupd, tenvupd) = transDecs venv tenv decs in
            transExp venvupd tenvupd exp
        | A.IfExp (ifexp, thenexp, elseexpopt, pos) ->
          let () = checkInt (trexp ifexp) pos in
          let thenty = unwrap (trexp thenexp) in
          let () = match elseexpopt with
            | Some elseexp -> checkTyEqOrNilSym (unwrap (trexp elseexp)) thenty pos
            | None -> checkTyEq thenty Types.Unit pos in
          wrap thenty
        | A.WhileExp (whileexp, loopexp, pos) ->
          checkInt (trexp whileexp) pos;
          checkTyEq (unwrap (trexp loopexp)) T.Unit pos;
          wrap T.Unit
        | A.ForExp(sym, _, assignexp, toexp, loopexp, pos) ->
          checkInt (trexp assignexp) pos;
          checkInt (trexp toexp) pos;
          let (venv', tenv') = transDecBody (venv, tenv) (A.VarDec
          {vname=sym; vescape=ref false; vtyp=None; init=assignexp; vpos=pos}) in
          checkTyEq (unwrap (transExp venv' tenv' loopexp)) T.Unit pos;
          wrap T.Unit
        | A.BreakExp (pos) -> wrap T.Unit
        | A.ArrayExp(sym, ctexp, initexp, pos) ->
          checkInt (trexp ctexp) pos;
          checkInt (trexp initexp) pos;
          wrap (getSym tenv sym pos)

    and check_record_types found expect pos =
        match (expect, found) with
        | ((esym, ety)::expct, (fsym, exp, pos)::fnd) -> 
          let () = checkSymEq esym fsym pos in
          let () = checkTyEqOrNil (unwrap (trexp exp)) ety pos in
          check_record_types fnd expct pos
        | ((s,t)::xs, []) -> raise (T.TypeNotFound (s, pos))
        | ([], (s,_,_)::xs) -> raise (T.TypeNotFound (s, pos))
        | ([], []) -> ()

    and check_func_call argtys exps pos =
        match (argtys, exps) with
        | ((_, ty)::args, e::exps) ->
          checkTyEq (unwrap (trexp e)) ty pos;
          check_func_call args exps pos
        | (x::xs, []) -> raise (T.TypeNotFound (Symbol.dummy, pos))
        | ([], x::xs) -> raise (T.TypeNotFound (Symbol.dummy, pos))
        | ([], []) -> ()

    and check_assignment var exp pos =
        let ty1 = unwrap (trvar var)
        and ty2 = unwrap (trexp exp) in
        let () = checkTyEqOrNil ty2 ty1 pos in
        wrap T.Unit

    and trvar = function
        | A.SimpleVar(id, pos) ->
            let Env.VarEntry ty = getSym venv id pos in
            wrap (T.actual_ty ty)
        | A.FieldVar (var, sym, pos) -> 
            let {exp=_; ty=T.Record (rcds, _)} = trvar var in
            get_record_type rcds sym
        | A.SubscriptVar (var, exp, pos) ->
            let {exp=_; ty=T.Array (ty, _)} = trvar var
            in checkInt (trexp exp) pos; wrap ty
    in trexp

and transDecHeader (venv, tenv) dec =
  let funcheader {A.fname; params; result; body; fpos} =
    (* add the function signature to venv *)
    let resty = match result with
      | Some (name, pos) -> getSym tenv name pos
      | None -> T.Unit 
    and transparam ({A.fldname; fldtyp; fldpos; _}: A.field) = (fldname, getSym tenv fldtyp fldpos) in
    let params' = List.map params transparam in
    let funentry = Env.FunEntry (params', resty) in
    (Symbol.enter venv fname funentry, tenv)

  in match dec with
    | A.FunctionDec fundec -> funcheader fundec
    | A.TypeDec (sym, A.RecordTy _, pos) ->
      (* Add dummy record entry *)
      (venv, Symbol.enter tenv sym (T.Name (sym, ref None)))
    | _ -> (venv, tenv)

and transDecBody (venv, tenv) dec =
  let funcbodyck {A.fname; body; fpos; _} =
    let Env.FunEntry (params, resty) = getSym venv fname fpos
    and enterparam env (name, ty) = Symbol.enter env name (Env.VarEntry ty) in
    let venv'' = List.fold_left params ~init:venv ~f:enterparam in
    let thing = transExp venv'' tenv body in
    checkTyEqOrNil (unwrap thing) resty fpos

  and update_record_refs fields pos =
    let update_fld (fldname, fldty) = match fldty with
      | T.Name (sym, tyopt) -> begin
        match !tyopt with
          | None -> tyopt := Some (getSym tenv sym pos)
          | _ -> ()
        end
      | _ -> () in
    let _ = List.map fields update_fld in ()

  in match dec with
    | A.FunctionDec fundec -> let () = funcbodyck fundec in (venv, tenv)
    | A.VarDec {vname; vtyp=tyopt; init=exp; _} -> 
      let {exp; ty} = transExp venv tenv exp in
      let () = match tyopt with 
        | Some (sym, pos) -> checkTyEqOrNil ty (getSym tenv sym pos) pos
        | None -> () in
      (Symbol.enter venv vname (Env.VarEntry ty), tenv)
    | A.TypeDec (sym, ty, pos) ->
      let trty = transTy tenv ty in
      match trty with
        | T.Record _ -> begin
          (* update tenv T.name entry with the type *)
          let T.Name (sym, tyopt) = getSym tenv sym pos in
          match !tyopt with
            | None -> tyopt := Some trty; (venv, tenv)
            | _ -> raise (GenericErr ("Redefinition of type " ^ (Symbol.name sym), pos))
          end
        | _ -> (venv, Symbol.enter tenv sym trty)

and transDecFinalise (venv, tenv) dec = (venv, tenv)

and transDecs venv tenv decs =
    let headersEnv = List.fold_left decs ~init:(venv, tenv) ~f:transDecHeader in
    let bodysEnv = List.fold_left decs ~init:headersEnv ~f:transDecBody in
    List.fold_left decs ~init: bodysEnv ~f:transDecFinalise 


let typeck ast =
    try
      let _ = transExp Env.base_venv Env.base_tenv ast in 
      print_endline "Type-checking successful"
    with
      | T.TypeNotFound (sym, pos) ->
        Format.printf "Error at %s - unknown type: %s@." (A.show_pos pos) (Symbol.name sym)
      | T.UnexpectedType (found, expect, pos) ->
        Format.printf "Error at %s - expected type %s but found type %s@." (A.show_pos pos) (T.show_ty expect) (T.show_ty found)
      | T.UnexpectedSymbol (found, expect, pos) ->
        Format.printf "Error at %s - expected symbol %s but found symbol %s@." (A.show_pos pos) (Symbol.name expect) (Symbol.name found)
      | GenericErr (s, pos) -> Format.printf "Error at %s - %s" (A.show_pos pos) s

let typeck_file fname =
    print_endline ("Type-checking file " ^ fname);
    typeck (Tigparse.parse (Lexing.from_channel (open_in fname)))
let typeck_string s = typeck (Tigparse.parse (Lexing.from_string s))

let () = if Array.length Sys.argv > 1 then
  let filename = Sys.argv.(1) in
  typeck_file filename
