
type sexp =
  | Atom of string
  | Exp of sexp * sexp
  | Nil

type program =
  | Cons of program * program
  | Head of program
  | Tail of program
  | X
  | ReturnNil

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
    if i>=cs.Length then Nil, i
    else
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
  | ReturnNil -> "Nil"

let examples =
  [|
    "a",        "a"
    "(a b)",      "(b a)"
    "(a (b c))",    "(c (b a))"
    "(a (b (c d)))",  "(d (c (b a)))"
  |]

let examples2 =
  [|
    "a",        ""
    "(a b)",      "a"
    "(a (b c))",    "(a b)"
    "(a (b (c d)))",  "(a (b c))"
  |]

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
  let subexprs = Seq.append [Nil, ReturnNil] (enumerate_sexps input) |> Map.ofSeq
  let rec construct sexp =
    if subexprs.ContainsKey sexp then subexprs.[sexp]
    else
      match sexp with
      | Exp(a, b) -> Cons(construct a, construct b)
      | Atom s -> failwith "Found unexpected symbol"
      | Nil -> failwith "Nil not supported"
  construct output

let predicate_generation i0 i1 =
  let rec pg i0 i1 f =
    match i0, i1 with
    | _, Atom _ -> None
    | Atom _, _ -> Some f
    | Exp(head0, tail0), Exp(head1, tail1) ->
      let p = pg head0 head1 (Head f)
      if p.IsSome then p
      else pg tail0 tail1 (Tail f)
    | _ -> failwith "unsupported case"
  pg i0 i1 X

let find_recurrence p0 p1 =
  let rec enumerate p = seq {
    yield p
    match p with
    | Cons(a, b) ->
        yield! enumerate a
        yield! enumerate b
    | Head a -> yield! enumerate a
    | Tail a -> yield! enumerate a
    | _ -> ()
  }
  let rec replace_x p sub =
    match p with
    | Cons(a, b) -> Cons(replace_x a sub, replace_x b sub)
    | Head a -> Head(replace_x a sub)
    | Tail a -> Tail(replace_x a sub)
    | X -> sub
    | ReturnNil -> ReturnNil
  let subs = enumerate p1 |> Set.ofSeq
  let replaced = subs |> Seq.map (fun sub -> replace_x p0 sub) |> Seq.toArray
  subs
  |> Seq.map (fun sub -> replace_x p0 sub, sub)
  |> Seq.filter (fun (p, _) -> p = p1)
  |> Seq.tryHead

let do_thing () =
  // Calculate the program fragments
  let print ((a, b), predicate, program) =
    let a, b = print_sexp a, print_sexp b
    let program = print_program program
    let predicate = defaultArg (Option.map print_program predicate) "default"
    sprintf "input: %s\noutput: %s\npredicate: %s\nprogram: %s" a b predicate program
  // Do all the stuff
  let examples =
    examples2 |> Array.map (fun (a, b) -> parse_old_sexp a, parse_old_sexp b)
  let predicates =
    let ps =
      examples
      |> Seq.map fst
      |> Seq.pairwise
      |> Seq.map (fun (i0, i1) -> predicate_generation i0 i1)
    Seq.append ps [None]
  let traces = examples |> Seq.map (fun (a, b) -> construct_fragment a b)
  // Return as printed string
  let v =
    Seq.zip3 examples predicates traces
    |> Seq.map print
    |> String.concat "\n\n"
  // ################
  traces
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> find_recurrence a b)
    |> Array.ofSeq

[<EntryPoint>]
let main argv =
  let s = do_thing ()
  0 // return an integer exit code
