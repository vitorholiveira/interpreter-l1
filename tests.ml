open OUnit2
open Printf
open Interpreter

(* Helper function to run tests *)
let test_expr expr expected_output =
  interpret expr = expected_output

(* Helper to create test cases *)
let make_test name expr expected =
  name >:: (fun _ -> assert_equal ~printer:(fun x -> x) expected (interpret expr))

let arithmetic_tests = "arithmetic" >::: [
  make_test "addition" 
    (Binop (Add, Num 5, Num 3))
    "8 : int";
    
  make_test "subtraction" 
    (Binop (Sub, Num 5, Num 3))
    "2 : int";
    
  make_test "multiplication" 
    (Binop (Mult, Num 5, Num 3))
    "15 : int";
    
  make_test "division" 
    (Binop (Div, Num 6, Num 2))
    "3 : int";
    
  make_test "division by zero" 
    (Binop (Div, Num 6, Num 0))
    "Runtime error: Division by zero";
]

let comparison_tests = "comparison" >::: [
  make_test "equal true" 
    (Binop (Eq, Num 5, Num 5))
    "true : bool";
    
  make_test "equal false" 
    (Binop (Eq, Num 5, Num 3))
    "false : bool";
    
  make_test "less than true" 
    (Binop (Lt, Num 3, Num 5))
    "true : bool";
    
  make_test "greater than false" 
    (Binop (Gt, Num 3, Num 5))
    "false : bool";
]

let logical_tests = "logical" >::: [
  make_test "and true" 
    (Binop (And, Bool true, Bool true))
    "true : bool";
    
  make_test "and false" 
    (Binop (And, Bool true, Bool false))
    "false : bool";
    
  make_test "or true" 
    (Binop (Or, Bool true, Bool false))
    "true : bool";
    
  make_test "or false" 
    (Binop (Or, Bool false, Bool false))
    "false : bool";
]

let conditional_tests = "conditional" >::: [
  make_test "if true" 
    (If (Bool true, Num 1, Num 2))
    "1 : int";
    
  make_test "if false" 
    (If (Bool false, Num 1, Num 2))
    "2 : int";
    
  make_test "if complex condition" 
    (If (Binop (Lt, Num 3, Num 5), Num 1, Num 2))
    "1 : int";
]

let let_binding_tests = "let binding" >::: [
  make_test "simple let" 
    (Let ("x", TyInt, Num 5, 
          Binop (Add, Var "x", Num 3)))
    "8 : int";
    
  make_test "nested let" 
    (Let ("x", TyInt, Num 5,
          Let ("y", TyInt, Num 3,
               Binop (Add, Var "x", Var "y"))))
    "8 : int";
]

let function_tests = "functions" >::: [
  make_test "simple function" 
    (Let ("f", TyFn (TyInt, TyInt),
          Fn ("x", TyInt, Binop (Add, Var "x", Num 1)),
          App (Var "f", Num 5)))
    "6 : int";
    
  make_test "higher order function" 
    (Let ("apply", TyFn (TyFn (TyInt, TyInt), TyFn (TyInt, TyInt)),
          Fn ("f", TyFn (TyInt, TyInt),
              Fn ("x", TyInt, App (Var "f", Var "x"))),
          App (App (Var "apply", 
                   Fn ("x", TyInt, Binop (Add, Var "x", Num 1))),
               Num 5)))
    "6 : int";
]

let list_tests = "lists" >::: [
  make_test "empty list" 
    (Nil TyInt)
    "nil:int : list int";
    
  make_test "cons list" 
    (Cons (Num 1, Cons (Num 2, Nil TyInt)))
    "(1 :: (2 :: nil:int)) : list int";
    
  make_test "head of list" 
    (Hd (Cons (Num 1, Cons (Num 2, Nil TyInt))))
    "1 : int";
    
  make_test "tail of list" 
    (Tl (Cons (Num 1, Cons (Num 2, Nil TyInt))))
    "(2 :: nil:int) : list int";
    
  make_test "is_empty true" 
    (IsEmpty (Nil TyInt))
    "true : bool";
    
  make_test "is_empty false" 
    (IsEmpty (Cons (Num 1, Nil TyInt)))
    "false : bool";
]

let maybe_tests = "maybe" >::: [
  make_test "nothing" 
    (Nothing TyInt)
    "nothing:int : maybe int";
    
  make_test "just" 
    (Just (Num 5))
    "just(5) : maybe int";
    
  make_test "match nothing" 
    (MatchMaybe (Nothing TyInt,
                Num 0,
                "x",
                Var "x"))
    "0 : int";
    
  make_test "match just" 
    (MatchMaybe (Just (Num 5),
                Num 0,
                "x",
                Var "x"))
    "5 : int";
]

let type_error_tests = "type errors" >::: [
  make_test "add bool" 
    (Binop (Add, Bool true, Num 3))
    "Type error: Arithmetic operation expects int arguments, got bool and int";
    
  make_test "compare bool" 
    (Binop (Lt, Bool true, Bool false))
    "Type error: Comparison operation expects int arguments, got bool and bool";
    
  make_test "and with numbers" 
    (Binop (And, Num 1, Num 0))
    "Type error: Logical operation expects bool arguments, got int and int";
]

let recursive_function_tests = "recursion" >::: [
  make_test "factorial" 
    (LetRec ("fact", TyFn (TyInt, TyInt),
             Fn ("n", TyInt,
                 If (Binop (Eq, Var "n", Num 0),
                     Num 1,
                     Binop (Mult, Var "n", App (Var "fact", Binop (Sub, Var "n", Num 1))))),
             App (Var "fact", Num 5)))
    "120 : int";

  make_test "fibonacci" 
    (LetRec ("fib", TyFn (TyInt, TyInt),
             Fn ("n", TyInt,
                 If (Binop (Lt, Var "n", Num 2),
                     Var "n",
                     Binop (Add, App (Var "fib", Binop (Sub, Var "n", Num 1)),
                                 App (Var "fib", Binop (Sub, Var "n", Num 2))))),
             App (Var "fib", Num 6)))
    "8 : int";
]

let complex_match_tests = "complex pattern matching" >::: [
  make_test "nested match list" 
    (MatchList (Cons (Num 1, Cons (Num 2, Cons (Num 3, Nil TyInt))),
                 Num 0,
                 "x", "xs",
                 Binop (Add, Var "x", MatchList (Var "xs",
                                                Num 0,
                                                "y", "ys",
                                                Binop (Add, Var "y", Num 100)))))
    "103 : int";

  make_test "nested match maybe" 
    (MatchMaybe (Just (Just (Num 42)),
                 Num 0,
                 "x",
                 MatchMaybe (Var "x", Num 0, "y", Binop (Add, Var "y", Num 1))))
    "43 : int";
]

let unbound_variable_tests = "unbound variables" >::: [
  make_test "unbound variable in binop" 
    (Binop (Add, Var "x", Num 5))
    "Type error: Unbound variable x";

  make_test "unbound variable in function" 
    (App (Fn ("x", TyInt, Binop (Add, Var "x", Var "y")), Num 3))
    "Type error: Unbound variable y";
]

let edge_case_tests = "edge cases" >::: [
  make_test "empty list in head" 
    (Hd (Nil TyInt))
    "Runtime error: Cannot take head of an empty list";

  make_test "empty list in tail" 
    (Tl (Nil TyInt))
    "Runtime error: Cannot take tail of an empty list";

  make_test "division by negative number" 
    (Binop (Div, Num 10, Num (-2)))
    "-5 : int";
]

let suite = "all_tests" >::: [
  arithmetic_tests;
  comparison_tests;
  logical_tests;
  conditional_tests;
  let_binding_tests;
  function_tests;
  list_tests;
  maybe_tests;
  type_error_tests;
  recursive_function_tests;
  complex_match_tests;
  unbound_variable_tests;
  edge_case_tests;
]

(* Run the tests *)
let () = 
  run_test_tt_main suite