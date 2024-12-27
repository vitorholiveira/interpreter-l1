open Printf

(* Custom exception types *)
exception TypeError of string
exception EvaluationError of string
exception ParserError of string

(* Basic types supported by the language *)
type ty =
  | TyInt     (* Integer type *)
  | TyBool    (* Boolean type *)
  | TyFn of ty * ty      (* Function type 'T1 -> T2' *)
  | TyMaybe of ty        (* Optional type 'Maybe T' *)
  | TyList of ty         (* List type 'List T' *)

(* Identifiers are represented as strings *)
type ident = string

(* Supported binary operations *)
type op = 
  | Add | Sub | Mult | Div     (* Arithmetic *)
  | Eq | Gt | Lt | Geq | Leq   (* Comparison *)
  | And | Or                   (* Logical binary operations *)

(* Abstract syntax tree for expressions *)
type expr =
  | Num of int                                       (* Integer literal *)
  | Bool of bool                                     (* Boolean literal *)
  | Binop of op * expr * expr                        (* Binary operation *)
  | If of expr * expr * expr                         (* Conditional *)
  | Var of ident                                     (* Variable reference *)
  | App of expr * expr                               (* Function application *)
  | Fn of ident * ty * expr                          (* Function definition *)
  | Let of ident * ty * expr * expr                  (* Let binding *)
  | LetRec of ident * ty * expr * expr               (* Recursive let *)
  | Nil of ty                                        (* Empty list *)
  | Cons of expr * expr                              (* List constructor *)
  | IsEmpty of expr                                  (* List empty check *)
  | Hd of expr                                       (* List head *)
  | Tl of expr                                       (* List tail *)
  | MatchList of expr * expr * ident * ident * expr  (* List pattern matching *)
  | Nothing of ty                                    (* Optional nothing *)
  | Just of expr                                     (* Optional value *)
  | MatchMaybe of expr * expr * ident * expr         (* Optional pattern matching *)

(* Type environment maps identifiers to their types *)
type type_env = (ident * ty) list

(* Runtime values *)
type value =
  | VNum of int
  | VBool of bool
  | VClosure of ident * expr * runtime_env   (* Function closure *)
  | VRecClosure of ident * ident * expr * runtime_env  (* Recursive closure *)
  | VNothing of ty (* nothing: T *)
  | VJust of value (* just v *)
  | VNil of ty (* nil: T *)
  | VList of value * value (* v1 :: v2 *)
and
  (* Runtime environment maps identifiers to their values *)
  runtime_env = (ident * value) list

(* Environment operations *)
let lookup_type (env: type_env) (x: ident) : ty option =
  List.assoc_opt x env

let lookup_value (env: runtime_env) (x: ident) : value option =
  List.assoc_opt x env

let extend_env env key value = 
  (key, value) :: env

(* String conversion for types *)
let rec string_of_type = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFn (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_type t1) (string_of_type t2)
  | TyMaybe t -> Printf.sprintf "maybe %s" (string_of_type t)
  | TyList t -> Printf.sprintf "list %s" (string_of_type t)

(* String conversion for values *)
let rec string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure _ -> "<function>"
  | VRecClosure _ -> "<recursive function>"
  | VNothing t -> Printf.sprintf "nothing:%s" (string_of_type t)
  | VJust v -> Printf.sprintf "just(%s)" (string_of_value v)
  | VNil t -> Printf.sprintf "nil:%s" (string_of_type t)
  | VList (v1, v2) -> Printf.sprintf "(%s :: %s)" (string_of_value v1) (string_of_value v2)

(*===============================================================================================================*)

(*================*)
(* Type inference *)
(*================*)

let rec type_infer (env: type_env) (e: expr) : ty =
  let type_error msg = raise (TypeError msg) in
  match e with
  (* T-Int  *)
  | Num _ -> TyInt
  (* T-Bool *)
  | Bool _ -> TyBool
  (* T-If *)
  | If (e1, e2, e3) ->
      (match type_infer env e1 with
      | TyBool ->
          let t2 = type_infer env e2 in
          let t3 = type_infer env e3 in
          if t2 = t3 then t2
          else type_error "If branches have different types"
      | t -> type_error (Printf.sprintf "If condition must be bool, got %s" (string_of_type t)))
  (* T-Var *)
  | Var x ->
      (match lookup_type env x with
      | Some t -> t
      | None -> type_error (Printf.sprintf "Unbound variable %s" x))
  (* T-App *)
  | App (e1, e2) ->
      (match type_infer env e1 with
      | TyFn (t1, t2) ->
          let t_arg = type_infer env e2 in
          if t1 = t_arg then t2
          else type_error "Function argument type mismatch"
      | t -> type_error (Printf.sprintf "Expected function type, got %s" (string_of_type t)))
  (* T-Fn *)
  | Fn (x, t, e) ->
      let t_body = type_infer (extend_env env x t) e in
      TyFn (t, t_body)
  (* T-Let *)
  | Let (x, t, e1, e2) ->
      let t1 = type_infer env e1 in
      if t1 <> t then
        type_error (Printf.sprintf "Let binding type mismatch: expected %s, got %s"
          (string_of_type t) (string_of_type t1));
      type_infer (extend_env env x t) e2
  (* T-LetRec *)
  | LetRec (f, t, e1, e2) ->
      (match t with
      | TyFn (t1, t2) ->
          let env' = extend_env env f t in
          let t_body = type_infer env' e1 in
          if t_body <> t then
            type_error (Printf.sprintf "Recursive function body type mismatch. Body: %s // Expected: %s" (string_of_type t_body) (string_of_type t));
          type_infer env' e2
      | _ -> type_error "LetRec requires function type")
  (* T-Nil *)
  | Nil t -> TyList t
  (* T-Cons *)
  | Cons (e1, e2) ->
      let t1 = type_infer env e1 in
      (match type_infer env e2 with
      | TyList t2 when t1 = t2 -> TyList t1
      | _ -> type_error "Cons arguments type mismatch")
  (* T-Empty *)
  | IsEmpty e ->
      (match type_infer env e with
      | TyList _ -> TyBool
      | t -> type_error (Printf.sprintf "IsEmpty expects list, got %s" (string_of_type t)))
  (* T-Head *)
  | Hd e ->
    (match type_infer env e with
    | TyList t -> t
    | t -> type_error (Printf.sprintf "Head expects list, got %s" (string_of_type t)))
  (* T-Tail *)
  | Tl e ->
      (match type_infer env e with
      | TyList t -> TyList t
      | t -> type_error (Printf.sprintf "Tail expects list, got %s" (string_of_type t)))
  (* T-MatchList *)
  | MatchList (e, e1, x, xs, e2) ->
      (match type_infer env e with
      | TyList t ->
          let t1 = type_infer env e1 in
          let t2 = type_infer (extend_env (extend_env env x t) xs (TyList t)) e2 in
          if t1 = t2 then t1
          else type_error "Match branches have different types"
      | t -> type_error (Printf.sprintf "Match expects list, got %s" (string_of_type t)))
  (* T-Nothing *)
  | Nothing t -> TyMaybe t
  (* T-Just *)
  | Just e -> TyMaybe (type_infer env e)
  (* T-MatchMaybe *)
  | MatchMaybe (e, e1, x, e2) ->
      (match type_infer env e with
      | TyMaybe t ->
          let t1 = type_infer env e1 in
          let t2 = type_infer (extend_env env x t) e2 in
          if t1 = t2 then t1
          else type_error "Match branches have different types"
      | t -> type_error (Printf.sprintf "Match expects maybe, got %s" (string_of_type t)))
  (* T-Binop_ *)
  | Binop (oper, e1, e2) ->
    let t1 = type_infer env e1 in
    let t2 = type_infer env e2 in
    match oper with
    | Add | Sub | Mult | Div ->
        if t1 <> TyInt || t2 <> TyInt then
          type_error (Printf.sprintf "Arithmetic operation expects int arguments, got %s and %s" (string_of_type t1) (string_of_type t2));
        TyInt
    | Eq | Lt | Gt | Geq | Leq ->
        if t1 <> TyInt || t2 <> TyInt then
          type_error (Printf.sprintf "Comparison operation expects int arguments, got %s and %s" (string_of_type t1) (string_of_type t2));
        TyBool
    | And | Or ->
        if t1 <> TyBool || t2 <> TyBool then
          type_error (Printf.sprintf "Logical operation expects bool arguments, got %s and %s" (string_of_type t1) (string_of_type t2));
        TyBool
(*===============================================================================================================*)

(*============*)
(* EVALUATION *)
(*============*)

(*Evaluation of binary operations *)
let eval_binop op v1 v2 =
  match (op, v1, v2) with
  | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
  | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
  | (Mult, VNum n1, VNum n2) -> VNum (n1 * n2)
  | (Div, VNum n1, VNum n2) when n2 <> 0 -> VNum (n1 / n2)
  | (Div, VNum _, VNum 0) -> raise (EvaluationError "Division by zero")
  | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
  | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
  | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
  | (Geq, VNum n1, VNum n2) -> VBool (n1 >= n2)
  | (Leq, VNum n1, VNum n2) -> VBool (n1 <= n2)
  | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
  | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
  | _ -> raise (EvaluationError "Invalid operand types for binary operation")

(* Expression evaluation *)
let rec eval (env: runtime_env) (e: expr) : value =
  match e with
  (* E-Num *)
  | Num n -> VNum n
  (* E-Bool *)
  | Bool b -> VBool b
  (* E-Binop *)
  | Binop (op, e1, e2) -> eval_binop op (eval env e1) (eval env e2)
  (* E-Var *)
  | Var x ->
      (match lookup_value env x with
      | Some v -> v
      | None -> raise (EvaluationError (Printf.sprintf "Unbound variable %s" x)))
  (* E-IfTrue and E-IfFalse *)
  | If (e1, e2, e3) ->
    (match eval env e1 with
    | VBool true -> eval env e2
    | VBool false -> eval env e3
    | _ -> raise (EvaluationError "If condition must evaluate to boolean"))
  (* E-App *)
  | App (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1 with
      | VClosure (x, body, env') ->
          eval (extend_env env' x v2) body
      | VRecClosure (f, x, body, env') ->
          eval (extend_env (extend_env env' f v1) x v2) body
      | _ -> raise (EvaluationError "Application of non-function value"))
  (* E-Fn *)
  | Fn (x, _, body) -> VClosure (x, body, env)
  (* E-Let *)
  | Let (x, _, e1, e2) ->
      let v1 = eval env e1 in
      eval (extend_env env x v1) e2
  (* E-LetRec *)
  | LetRec (f, _, Fn (x, _, body), e2) ->
      let closure = VRecClosure (f, x, body, env) in
      eval (extend_env env f closure) e2
  | LetRec _ -> raise (ParserError "LetRec must bind function")

  (* E-Nil *)
  | Nil t -> VNil t
  (* E-Cons *)
  | Cons (e1, e2) -> VList (eval env e1, eval env e2)
  (* E-IsEmpty *)
  | IsEmpty e ->
      (match eval env e with
      | VNil _ -> VBool true
      | VList _ -> VBool false
      | _ -> raise (EvaluationError "IsEmpty applied to non-list"))
  (* E-Head*)
  | Hd e ->
      (match eval env e with
      | VList (v, _) -> v
      | VNil _ -> raise (EvaluationError "Cannot take head of an empty list")
      | _ -> raise (EvaluationError "Head applied to non-list"))
  (* E-Tail*)
  | Tl e ->
      (match eval env e with
      | VList (_, v) -> v
      | VNil _ -> raise (EvaluationError "Cannot take tail of an empty list")
      | _ -> raise (EvaluationError "Tail applied to non-list"))
  (* E-MatchListNil and E-MatchList *)
  | MatchList (e, e1, x, xs, e2) ->
      (match eval env e with
      | VNil _ -> eval env e1
      | VList (v1, v2) ->
          eval (extend_env (extend_env env x v1) xs v2) e2
      | _ -> raise (EvaluationError "Match applied to non-list"))
  (* E-Nothing *)
  | Nothing t -> VNothing t
  (* E-Just *)
  | Just e -> VJust (eval env e)
  (* E-MatchMaybeNothing and E-MatchMaybeJust*)
  | MatchMaybe (e, e1, x, e2) ->
      (match eval env e with
      | VNothing _ -> eval env e1
      | VJust v -> eval (extend_env env x v) e2
      | _ -> raise (EvaluationError "Match applied to non-maybe"))

(*===============================================================================================================*)

(* Main interpreter function *)
let interpret (e: expr) : string =
  try
    let ty = type_infer [] e in
    let value = eval [] e in
    Printf.sprintf "%s : %s" (string_of_value value) (string_of_type ty)
  with
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | EvaluationError msg -> Printf.sprintf "Runtime error: %s" msg
  | ParserError msg -> Printf.sprintf "Parser error: %s" msg