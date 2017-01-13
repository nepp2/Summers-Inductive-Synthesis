
// car = head
// cdr = tail

let head = List.head
let tail = List.tail
let cons (x, y) = x :: y

(*
let b =
  [a(tail(x)) , []
   a(tail(tail(x))), cons(head(x), [])
   a(tail(tail(tail(x)))), cons(head(x), cons(head(tail(x)), []))
   T, cons(car(x), cons(head(tail(x)), cons(head(tail(tail(x))), [])))]
*)

type sexp =
  | Atom of string
  | Exp of sexp * sexp
  | Nil

type program =
  | Cons of program * program
  | Head of program
  | Tail of program
  | X

let parse_old_sexp (s : string) =
  let cs = s.ToCharArray()
  let rec parse_word i : int =
    if i>=cs.Length then i
    else
      match cs.[i] with
      | '(' -> i | ')' -> i | ' ' -> i
      | _ -> parse_word (i + 1)
  let rec skip_whitespace i : int =
    if cs.[i] = ' ' then skip_whitespace (i + 1)
    else i
  let rec close_paren i : int =
    if cs.[i] = ')' then i + 1
    else failwith "expected close paren"
  let rec parse_sexp i : sexp * int =
    match cs.[i] with
    | '(' ->
      let i = skip_whitespace (i + 1)
      let a, i = parse_sexp i
      let i = skip_whitespace i
      let b, i = parse_sexp i
      let i = skip_whitespace i
      let i = close_paren i
      Exp(a, b), i
    | ')' -> failwith "unexpected close paren"
    | ' ' -> parse_sexp (i + 1)
    | _ ->
      let end_index = parse_word i
      Atom (cs.[i..end_index-1] |> System.String), end_index
  parse_sexp 0 |> fst

let parse_modern_sexp (s : string) =
  let cs = s.ToCharArray()
  let rec parse_word i : int =
    match cs.[i] with
    | '(' -> i | ')' -> i | ' ' -> i
    | _ -> parse_word (i + 1)
  let rec parse_sexp i : sexp * int =
    // Deals with finding and skipping the first paren
    match cs.[i] with
    | '(' -> parse_list (i + 1)
    | ' ' -> parse_sexp (i + 1)
    | _ -> failwith "not implemented"
  and parse_list i : sexp * int =
    // Parses all the things
    match cs.[i] with
    | '(' ->
      let head, i = parse_sexp i
      let tail, i = parse_list i
      Exp (head, tail), i
    | ')' ->
      Nil, i + 1
    | ' ' ->
      parse_list (i + 1)
    | _ ->
      let end_index = parse_word i
      let head = Atom (cs.[i..end_index-1] |> System.String)
      let tail, i = parse_list end_index
      Exp (head, tail), i
      
  parse_sexp 0 |> fst

let rec print_sexp sexp =
  match sexp with
  | Atom s -> s
  | Exp(a, b) -> sprintf "(%s . %s)" (print_sexp a) (print_sexp b)
  | Nil -> ""

let rec print_program p =
  match p with
  | Cons(a, b) -> sprintf "cons(%s, %s)" (print_program a) (print_program b)
  | Head a -> sprintf "head(%s)" (print_program a)
  | Tail a -> sprintf "tail(%s)" (print_program a)
  | X -> "X"

let examples =
  [
    "a",        "a"     // (a(tail(tail(c)) -> []
    "(a b)",      "(b a)"
    "(a (b c))",    "(c (b a))"
    "(a (b (c d)))",  "(d (c (b a)))"
  ] //TODO |> List.map to_example

let enumerate_sexps sexp =
  let rec enumerate sexp bf = seq {
    match sexp with
    | Exp(a, b) ->
      yield sexp, bf
      yield! enumerate a (Head bf)
      yield! enumerate b (Tail bf)
    | Atom s -> yield sexp, bf
    | Nil -> ()
  }
  enumerate sexp X |> Seq.toArray
  
let construct_fragment input output =
  let subexprs = enumerate_sexps input |> Map.ofSeq
  let rec construct sexp =
    if subexprs.ContainsKey sexp then subexprs.[sexp]
    else
      match sexp with
      | Exp(a, b) -> Cons(construct a, construct b)
      | Atom s -> failwith "Found unexpected symbol"
      | Nil -> failwith "Nil not supported"
  construct output
  
let do_thing () =
  let parse (a, b) =
    (parse_old_sexp a), (parse_old_sexp b)
  let construct (a, b) =
    let program = construct_fragment a b
    a, b, program
  let print (a, b, program) =
    let a, b = print_sexp a, print_sexp b
    let program = print_program program
    sprintf "input: %s\noutput: %s\nprogram: %s" a b program
  examples
  |> Seq.map (parse >> construct >> print)
  |> String.concat "\n\n"
                

[<EntryPoint>]
let main argv =
  let s = do_thing ()
  0 // return an integer exit code
