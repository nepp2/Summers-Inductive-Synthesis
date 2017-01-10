
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
  | Cons of value * sexp
  | Nil
and value =
  | Atom of obj
  | Sexp of sexp

type basic_function =
  | Head of basic_function
  | Tail of basic_function
  | X

let parse_sexp (s : string) =
  let cs = s.ToCharArray()
  let rec parse_word i : int =
    match cs.[i] with
    | '(' -> i | ')' -> i | ' ' -> i
    | _ -> parse_word (i + 1)
  let rec parse_sexp i : sexp * int =
    // Deals with finding and skipping the first paren
    match cs.[i] with
    | '(' ->
      let vals, i = parse_list [] (i + 1)
      (vals |> List.fold (fun t h -> Cons(h, t)) Nil), i
    | ' ' -> parse_sexp (i + 1)
    | _ -> failwith "not implemented"
  and parse_list values i : value list * int =
    // Parses all the things
    match cs.[i] with
    | '(' ->
      let head, i = parse_sexp i
      parse_list ((Sexp head) :: values) i
    | ')' ->
      values, i + 1
    | ' ' ->
      parse_list values (i + 1)
    | _ ->
      let end_index = parse_word i
      let head = Atom (cs.[i..end_index] |> System.String)
      parse_list (head :: values) end_index
    
  parse_sexp 0

let print_sexp sexp =
  let l = new System.Collections.Generic.List<string> ()
  let rec print_list
  let rec print_sexp sexp =
    match sexp with
    | Cons(h, t) ->
      l.Add "("
      l.Add ")"
    | Nil -> l.Add "()"
  print sexp
  String.concat "" l

let rec to_expr(ls : obj list) =
  match ls with
  | [] -> Nil
  | x :: xs ->
    match box x with
    | :? list<obj> as a ->
      let v = to_expr a
      Cons(Sexp v, to_expr xs)
    | _ -> Cons(Atom x, to_expr xs)

type example = { input : sexp ; output : sexp }

let to_example (input, output) =
  { input = input |> List.map box |> to_expr ; output = output |> List.map box |> to_expr }

let examples =
  [
    "(a)",        "()"     // (a(tail(tail(c)) -> []
    "(a b)",      "(a)"
    "(a b c)",    "(a b)"
    "(a b c d)",  "(a b c)"
  ] //TODO |> List.map to_example

let enumerate_sexps sexp =
  let rec enumerate sexp bf = seq {
    yield Sexp sexp, bf
    match sexp with
    | Cons(value, rest) ->
      yield! enumerate rest (Tail bf)
      match value with
      | Atom a -> yield value, (Head bf)
      | Sexp s -> yield! enumerate s (Head bf)
    | Nil -> yield Sexp Nil, bf
  }
  enumerate sexp X |> Seq.toArray
  


[<EntryPoint>]
let main argv =
  let v, i = parse_sexp "((abba b) (c d))"
  let s = sprintf "%A" v
  //let v = to_expr [['a' ; 'b'] ; ['c' ; 'd']]
  0 // return an integer exit code
