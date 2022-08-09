(* Simple interpreter based on Denotational Semantics.  

   Based on the paper by R. D. Tennent

*)

type value
  = Int of int
  | Bool of bool

type expr = 
  | Val of value
  | Id  of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Lt  of expr * expr
  | Lte of expr * expr
  | Gt  of expr * expr
  | Gte of expr * expr
  | Eq  of expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr


(* In our language, variables must be integers.  Thus the state /
   environment maps names (strings) to integer values. *)

type environment = (string * int) list

let rec lookup (name: string) (env: environment) : int =
  match env with 
  | [ ] -> raise (Failure ("Name \"" ^ name ^ "\" not found."))
  | (k,v)::rest -> if name = k then v else lookup name rest

let rec eval (e: expr) (env: environment) : value = 
  match e with
  | Val v -> v
  | Id  n -> Int (lookup n env)
  | Add (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "incompatible types, Sub")
     )
  | Mul (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int 0 -> raise (Failure "error, DivisionByZero")
       | Int v1, Int v2 -> Int (v1 / v2)
       | _ -> raise (Failure "incompatible types, Div")
     )
  | Lt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _ -> raise (Failure "incompatible types, Lt")
     )
  | Lte (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 <= v2)
       | _ -> raise (Failure "incompatible types, Lte")
     )
  | Gt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 > v2)
       | _ -> raise (Failure "incompatible types, Gt")
     )
  | Gte (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 >= v2)
       | _ -> raise (Failure "incompatible types, Gte")
     )
  | Not (e1) ->
     ( match eval e1 env with
       | Bool b1 -> Bool (not b1)
       | _ -> raise (Failure "incompatible type, Not")
     )
  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _ -> raise (Failure "incompatible type, And")
     )
  | Or (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool b1, Bool b2 -> Bool (b1 || b2)
       | _ -> raise (Failure "incompatible type, Or")
     )

  | _ -> raise (Failure "Fill in the missing cases in `eval`")


(* A statement has the expected forms from imperative programming. *)
type stmt =
  | Skip 
  | Assign of string * expr
  | Seq of stmt * stmt
  | IfThenElse of expr * stmt * stmt
  | While of expr * stmt

  | ReadNum of string
  | WriteNum of expr
  | WriteStr of string

  | IfThen of expr * stmt
  | For of string * expr * expr * stmt
  | RepeatUntil of stmt * expr
  | DoWhile of stmt * expr

let rec read_number () : int =
  try int_of_string (read_line ()) with
  | Failure _ ->
     print_endline "Please, enter an integer value: ";
     read_number ()

type state = environment

let rec exec (s: stmt) (stt: state) : state =
  match s with
  | Skip -> stt

  | Assign (nm, e) ->
     (match eval e stt with
      | Int i ->  (nm, i) :: stt
      | Bool _ -> failwith "Bools not allowed in assignments"
     )

  | Seq (s1, s2) -> exec s2 (exec s1 stt)

  | ReadNum (nm) -> (nm, read_number ()) :: stt

  | WriteNum (e1) ->
     (match eval e1 stt with
      | Int i -> print_endline(Int.to_string i); stt
      | Bool _ -> failwith "Incompatible type in WriteNum"
     )

  | WriteStr (str) -> print_endline(str); stt
  
  | IfThenElse (cond, ts, es) ->
     (match eval cond stt with
      | Bool true  -> exec ts stt
      | Bool false -> exec es stt
      | Int _ -> failwith "Incompatible types in IfThenElse"
     )

  | While (cond, body) ->
     (match eval cond stt with
      | Bool true  -> exec (While (cond, body)) (exec body stt)
      | Bool false -> stt
      | Int _ -> failwith "Incompatible types in While"
     )     

  | DoWhile (body, cond) -> 
     (match eval cond stt with
     | Bool _ -> exec (While (cond, body)) (exec body stt)
     | Int _ -> failwith "Incompatible types in DoWhile"
     )
  
  | RepeatUntil (body, cond) ->
     (match eval cond stt with
      | Bool _ -> exec (While (Not(cond), body)) (exec body stt)
      | Int _ -> failwith "Incompatible types in RepeatUntil"
     )

  | For (index, i, j, body) ->
     (match eval i stt, eval j stt with
      | Int i, Int j -> exec (Seq(Assign(index, Val(Int(i))),
                              While (Lte(Id index, Val(Int(j))), 
                                Seq(body, Assign(index, Add(Id index, Val(Int(1)) )))
                                  ))) stt
      | _, _ -> failwith "Incompatible types in For"
     )
  
  | _ -> raise (Failure "Fill in the missing cases in `exec`")
         (* Once you've filled in the missing cases you can remove this. *)


(* A program has 
   - a name, 
   - a list of variables used in it, and
   - a statement.
 *)
type prog = string * string list * stmt


(* This function will initialize the program state and run it.
 *)
let run (p: prog) : state =
  let (_, vars, s) = p 
  in
  let init_state = List.map (fun v -> (v, 0)) vars
  in
  exec s init_state



(* Some sample programs *)
let program_assign =
  (* x := 1;
     y := x + 2;
   *)
  ("assign", 
   ["x"; "y"],
   Seq (Assign ("x", Val (Int 1)),
        Assign ("y", Add (Id  "x", Val (Int 2)))
     )
  )


let program_seq =
  (*   x := 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
       x := x + 1 ;
   *)
  ("program_seq", ["x"],
  Seq (Assign ("x", Val (Int 1)),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
  Seq (Assign ("x", Add (Id "x", Val (Int 1))),
       Assign ("x", Add (Id "x", Val (Int 1))))))))
  )


let program_ifthenelse_simple =
  (* y = 10
     if y < 15 then result := 10 else result := 11
   *)
  ("ifthen_else_simple",
   ["y"; "result"],
   Seq (Assign ("y", Val (Int 10)),
        IfThenElse 
          (Lt (Id  "y", Val (Int 15)),
           Assign ("result", Val (Int 10)),
           Assign ("result", Val (Int 11))
          )
     )
  )



(* A few versions of summing up to 10 using different forms of loops. *)
let program_sum_to_10_while =
  (* i = 0;
     sum = 0;
     while (i <= 10) {
       sum = sum + i;
       i = i + 1
     }
   *)
  ("sum_10_while",
   ["i"; "sum"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (While (Lte (Id  "i", Val (Int 10)),
               Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
                    Assign ("i", Add (Id  "i", Val (Int 1))) ) 
          ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       )
     )
     )
     )
  )

let program_sum_to_10_repeat =
  (* i = 0;
     sum = 0
     repeat
       sum = sum + i
       i = i + 1
     until i > 10;
     write "sum is:";
     write sum
   *)
  ("sum_10_repeat",
   ["i"; "sum"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (RepeatUntil (Seq (Assign("sum", Add (Id  "sum", Id  "i")),
                          Assign ("i", Add (Id  "i", Val (Int 1)))
                       ),
                     Gt (Id "i", Val (Int 10))
          ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       )
     )
     )
     )
  )

let program_sum_to_10_do_while =
  (* i = 0;
     sum = 0;
     do
       sum = sum + i;
       i = i + 1
     while (i <= 10) 
   *)
  ("sum",
   ["i"; "sum_10_do_while"],
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (DoWhile (Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
                      Assign ("i", Add (Id  "i", Val (Int 1))) 
                     ),
                 Lte (Id  "i", Val (Int 10))
                ),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
     )
     )
     )
     )
  )

let program_sum_to_10_for =
  (* sum = 0
     for i = 0 to 10
       sum = sum + i
     write "sum is:";
     write sum
   *)
  ("sum_10_for",
   ["sum"; "i"],
   Seq (Assign ("sum", Val (Int 0)),
   Seq (For ("i", Val (Int 0), Val (Int 10),
             Assign("sum", Add (Id  "sum", Id  "i"))),
   Seq (WriteStr "sum is",
        WriteNum (Id  "sum")
       ) ) )
  )


let run_sums _ =
  ignore (run program_sum_to_10_while);
  ignore (run program_sum_to_10_repeat);
  ignore (run program_sum_to_10_do_while);
  ignore (run program_sum_to_10_for);
  ()

(* A simple for loop. *)
let program_for =
  (* for i = 1 to 5
       write i
   *)
  ("for",
   ["i"],
   For ("i", Val (Int 1), Val (Int 5), WriteNum (Id  "i"))
  )






(* Some interactive programs *)
let program_seq_read_write =
  (* write "Enter a value for x:";
     read x;
     y := x + 2;
     z := y * 3;
     write "value of z is:";
     write z
   *) 
  ("seq",
   ["x"; "y"; "z"],
   Seq (WriteStr "Enter a value for x:",
   Seq (ReadNum "x",
   Seq (Assign ("y", Add (Id  "x", Val (Int 2))),
   Seq (Assign ("z", Mul (Id  "y", Val (Int 3))),
   Seq (WriteStr "value of z is:",
        WriteNum (Id  "z")
       ) ) ) ) )
  )

let program_sum =
  (* write "Enter a number to sum to:";
     read x;
     i = 0;
     sum = 0;
     while (i < x) {
       write i;
       sum = sum + i;
       i = i + 1
     }
     write sum
   *)
  ("sum",
   ["x"; "i"; "sum"],
   Seq (WriteStr "Enter a number to sum to:",
   Seq (ReadNum "x",
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum", Val (Int 0)),
   Seq (While (Lt (Id  "i", Id  "x"),
               Seq (WriteNum (Id  "i"),
	       Seq (Assign ("sum", Add (Id  "sum", Id  "i")),
	            Assign ("i", Add (Id  "i", Val (Int 1)))
	           ) ) ),
        WriteNum (Id  "sum")
   ) ) ) ) )
  )

let program_sum_evens_odds =
  (* write "Enter a number to some sums of:";
     read x;
     i = 0;
     sum_evens = 0;
     sum_odds = 0;
     while (i < x) {
       write i;
       if i mod 2 = 0 then
          sum_evens = sum_evens + i;
       else
          sum_odds = sum_odds + i;
       i = i + 1
     }
     write "sum of evens is:";
     write sum_evens;
     write "sum of odds is:";
     write sum_odds
   *)
  ("sum_evens_odds",
   ["x"; "i"; "sum_evens"; "sum_odds"],
   Seq (WriteStr "Enter a number to some sums of:",
   Seq (ReadNum "x",
   Seq (Assign ("i", Val (Int 0)),
   Seq (Assign ("sum_evens", Val (Int 0)),
   Seq (Assign ("sum_odds", Val (Int 0)),
   Seq (While (
            Lt (Id  "i", Id  "x"),
 	    Seq (WriteNum (Id  "i"),
                 Seq (IfThenElse (
                          Eq (Mod (Id  "i", Val (Int 2)), Val (Int 0)),
                          Assign ("sum_evens", Add (Id  "sum_evens", Id  "i")),
                          Assign ("sum_odds", Add (Id  "sum_odds", Id  "i"))
                        ),
		      Assign ("i", Add (Id  "i", Val (Int 1)))
          ) ) ),
        Seq (WriteStr "sum of evens is:",
        Seq (WriteNum (Id  "sum_evens"), 
        Seq (WriteStr "sum of odds is:",
             WriteNum (Id  "sum_odds")
       ) ) ) ) ) ) ) ) )
  )
