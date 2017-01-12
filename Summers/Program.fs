
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

type sexp = value list
and value =
  | Atom of string
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
    | '(' -> parse_list (i + 1)
    | ' ' -> parse_sexp (i + 1)
    | _ -> failwith "not implemented"
  and parse_list i : value list * int =
    // Parses all the things
    match cs.[i] with
    | '(' ->
      let head, i = parse_sexp i
      let tail, i = parse_list i
      (Sexp head) :: tail, i
    | ')' ->
      [], i + 1
    | ' ' ->
      parse_list (i + 1)
    | _ ->
      let end_index = parse_word i
      let head = Atom (cs.[i..end_index-1] |> System.String)
      let tail, i = parse_list end_index
      head :: tail, i
      
  parse_sexp 0

let rec print_sexp sexp =
  let print_value v =
    match v with
    | Atom s -> s
    | Sexp s -> print_sexp s
  sprintf "(%s)" (sexp |> Seq.map print_value |> String.concat " ")

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
    | value :: rest ->
      yield! enumerate rest (Tail bf)
      match value with
      | Atom a -> yield value, (Head bf)
      | Sexp s -> yield! enumerate s (Head bf)
    | [] -> yield Sexp [], bf
  }
  enumerate sexp X |> Seq.toArray
  


[<EntryPoint>]
let main argv =
  let sexp, i = parse_sexp "((abba b) (c d))"
  let all_sexps = enumerate_sexps sexp |> Array.filter (fun (s, _) -> s <> Sexp [])
  let s = print_sexp sexp
  //let v = to_expr [['a' ; 'b'] ; ['c' ; 'd']]
  0 // return an integer exit code
