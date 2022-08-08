(* Types for values, expressions, and types in a small expression 
   language.
 *)

type value 
  = IntV of int
  | BoolV of bool

and  expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

  | Lt  of expr * expr
  | Lte  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Or  of expr * expr
  | Not of expr

  | Id of string
  | Let of string * typ * expr * expr

and  typ
  = IntT
  | BoolT

(* Evaluation *)

type environment = (string * value) list

exception DivisionByZero of value

let rec eval (e: expr) (env: environment) : value =
  match e with
  | Val v -> v 
  | Add (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> IntV (i1 + i2)
      | _ -> raise (Failure "Internal Error on Add")
     )
  | Sub (e1, e2) ->
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> IntV (i1 - i2)
      | _ -> raise (Failure "Internal Error on Sub")
     )
  | Mul (e1, e2) ->
    (match eval e1 env, eval e2 env with
     | IntV i1, IntV i2 -> IntV (i1 * i2)
     | _ -> raise (Failure "Internal Error on Mul")
    )
  | Div (e1, e2) ->
    (match eval e1 env, eval e2 env with
     | IntV i1, IntV 0 -> raise (DivisionByZero (IntV i1))
     | IntV i1, IntV i2 -> IntV (i1 / i2)
     | _ -> raise (Failure "Internal Error on Div")
    )
  | Lt (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> raise (Failure "Internal Error on Lt")
     )
  | Lte (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> BoolV (i1 <= i2)
      | _ -> raise (Failure "Internal Error on Lte")
     )
  | Eq (e1, e2) -> 
     (match eval e1 env, eval e2 env with
      | IntV i1, IntV i2 -> BoolV (i1 = i2)
      | _ -> raise (Failure "Internal Error on Eq")
     )
  | And (e1, e2) ->
     (match eval e1 env, eval e2 env with
      | BoolV b1, BoolV b2 -> BoolV(b1 && b2)
      | _ -> raise (Failure "Internal Error on And")
     )      
  | Or (e1, e2) ->
     (match eval e1 env, eval e2 env with
      | BoolV b1, BoolV b2 -> BoolV(b1 || b2)
      | _ -> raise (Failure "Internal Error on Or")
     )      
  | Not (e1) ->
     (match eval e1 env with
      | BoolV b1 -> BoolV(not b1)
      | _ -> raise (Failure "Internal Error on Not")
     )    
  | Id (str) ->
      let rec search_env s env = 
        (match env with
         | [] -> raise (Failure "Internal Error on Id")
         | (name,v)::xs -> if s = name then v else search_env s xs
        )
      in search_env str env
  | Let (s, ty, e1, e2) -> eval e2 ((s, eval e1 env)::env)



type context = (string * typ) list

type typ_error = ExpectedInt | ExpectedBool | UndeclaredName

type 'a result = Result of 'a
               | Error of typ_error

let rec type_check (e: expr) (ctx: context) : typ result =
  match e with
  | Val (IntV _) -> Result IntT
  | Val (BoolV _) -> Result BoolT
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) -> 
     (match type_check e1 ctx, type_check e2 ctx with
      | Result IntT, Result IntT -> Result IntT
      | Result BoolT, _ -> Error ExpectedInt
      | Result IntT, Result BoolT -> Error ExpectedInt
      | Error err, _ -> Error err
      | _, Error err -> Error err
     )
  | Lt (e1, e2) | Lte (e1, e2) | Eq (e1, e2) ->
     (match type_check e1 ctx, type_check e2 ctx with
      | Result IntT, Result IntT -> Result BoolT
      | Result BoolT, _ -> Error ExpectedInt
      | Result IntT, Result BoolT -> Error ExpectedInt
      | Error err, _ -> Error err
      | _, Error err -> Error err
     )
  | And (e1, e2) | Or (e1, e2) ->
     (match type_check e1 ctx, type_check e2 ctx with
      | Result BoolT, Result BoolT -> Result BoolT
      | Result IntT, _ -> Error ExpectedBool
      | _, Result IntT -> Error ExpectedBool
      | Error err, _ -> Error err
      | _, Error err -> Error err
     )
  | Not (e1) ->
     (match type_check e1 ctx with
      | Result BoolT -> Result BoolT
      | Result IntT -> Error ExpectedBool
      | Error err -> Error err
     )
  | Id (str) -> 
    let rec search_ctx s ctx = 
      (match ctx with
       | [] -> Error UndeclaredName
       | (name,ty)::xs -> if s = name then Result ty else search_ctx s xs
      )
    in search_ctx str ctx
  | Let (s, ty, e1, e2) ->
     (match type_check e1 ctx with
     | Result IntT when ty = IntT -> type_check e2 ((s,ty)::ctx)
     | Result BoolT when ty = BoolT -> type_check e2 ((s,ty)::ctx)
     | Result IntT -> Error ExpectedBool
     | Result BoolT -> Error ExpectedInt
     | Error err -> Error err
     )

let evaluate (e: expr) : value result =
  match type_check e [] with
  | Result ty -> Result (eval e [])
  | Error err -> Error err
