
type sexp =
  | Atom of string
  | Exp of sexp * sexp
  | Nil

type program =
  | Cons of program * program
  | Head of program
  | Tail of program
  | F of program
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
        let word = (cs.[i..end_index-1] |> System.String)
        if word = "NIL" then Nil, end_index
        else Atom word, end_index
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
  | F a -> sprintf "F(%s)" (print_program a)
  | X -> "X"
  | ReturnNil -> "Nil"

let examples =
  [|
    "NIL",        "NIL"
    "(a (b NIL))",      "(b NIL)"
    "((a (b NIL)) ((c (d NIL)) NIL))",    "(b (d NIL))"
    "((a (b NIL)) ((c (d NIL)) ((e (f NIL)) NIL)))",  "(b (d (e NIL)))"
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
    | _, Nil -> None
    | _, Atom _ -> None
    | Nil, _ -> Some f
    | Atom _, _ -> Some f
    | Exp(head0, tail0), Exp(head1, tail1) ->
      let p = pg head0 head1 (Head f)
      if p.IsSome then p
      else pg tail0 tail1 (Tail f)
    | _ -> failwith "unsupported case"
  pg i0 i1 X

let find_recurrence2 p0 p1 =
  let rec match_until_x a b = seq {
    match a, b with
    | Cons(ah, at), Cons(bh, bt) ->
        yield! match_until_x ah bh
        yield! match_until_x at bt
    | Head ah, Head bh ->
        yield! match_until_x ah bh
    | Tail at, Tail bt ->
        yield! match_until_x at bt
    | X, _ -> yield b
    | _ -> if a <> b then yield ReturnNil
  }
  let rec find_matching_subexprs a b = seq {
    let matches = match_until_x a b |> Array.ofSeq
    let matches = matches |> Seq.distinct |> Array.ofSeq
    if matches.Length = 1 then
      let h = Array.head matches
      if h <> ReturnNil then yield F(h)
    match b with
    | Cons(bh, bt) ->
        yield! find_matching_subexprs a bh |> Seq.map (fun p -> Cons(p, bt))
        yield! find_matching_subexprs a bt |> Seq.map (fun p -> Cons(bh, p))
    | Head bh -> yield! find_matching_subexprs a bh |> Seq.map Head
    | Tail bt -> yield! find_matching_subexprs a bt |> Seq.map Tail
    | _ -> ()
  }
  find_matching_subexprs p0 p1 |> Array.ofSeq  

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

let test_recurrence () =
  let examples =
    examples2 |> Array.map (fun (a, b) -> parse_old_sexp a, parse_old_sexp b)
  let traces = examples |> Seq.map (fun (a, b) -> construct_fragment a b)
  let recurrence_relation =
    traces
      |> Seq.pairwise
      |> Seq.map (fun (a, b) -> find_recurrence2 a b |> Set.ofArray)
      |> Seq.pairwise
      |> Seq.collect (fun (x, y) -> Set.intersect x y)
      |> Seq.tryLast
    
  ()

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
  Seq.zip3 examples predicates traces
  |> Seq.map print
  |> String.concat "\n\n"

[<EntryPoint>]
let main argv =
  //let s = do_thing ()
  test_recurrence ()
  0 // return an integer exit code
