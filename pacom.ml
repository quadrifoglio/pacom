module Err = struct
    type t =
        | Eof of int
        | Message of int * string
        | Nested of int * t
    
    let pos = function
        | Eof idx -> idx
        | Message (idx, _) -> idx
        | Nested (idx, _) -> idx
end

module Input = struct
    type t = (char list * int)

    let init str =
        (String.to_seq str |> List.of_seq, 1)

    let pos input =
        let (_, idx) = input in idx
end

module Parser = struct
    type 'a t = Input.t -> (('a * int) * Input.t, Err.t) result
end

let str_of_chrs chrs = List.to_seq chrs |> String.of_seq
let chrs_of_str str = String.to_seq str |> List.of_seq

let fail msg inp =
    let (_, idx) = inp in Error (Err.Message (idx, msg))

let return obj inp =
    let (_, idx) = inp in Ok ((obj, idx), inp)

let ( <|> ) p1 p2 =
    fun inp ->
        match p1 inp with
        | Ok (res, inp') -> Ok (res, inp')
        | Error (err) -> let start = Input.pos inp in
            if Err.pos err = start then p2 inp else Error (Err.Nested (start, err))

let ( >>= ) p f =
    fun inp ->
        let (_, start) = inp in
        match p inp with
        | Error err -> Error err
        | Ok (res, inp') ->
            (match f res inp' with
            | Error err -> Error (Err.Nested (start, err))
            | Ok ((res, _), rest) -> Ok ((res, start), rest))

let ( let* ) = ( >>= )

let ( <?> ) p f =
    p >>= fun (obj, _) -> if f obj then return obj else fail "predicate failed"

let ( => ) p f =
    p >>= fun (obj, _) -> return (f obj)

let ( >> ) p1 p2 =
    p1 >>= fun _ -> p2

let ( << ) p1 p2 =
    p1 >>= fun (obj, _) -> p2 >>= fun _ -> return obj

let ( <~> ) p1 p2 =
    p1 >>= fun (r, _) -> p2 >>= fun (rs, _) -> return (r :: rs)

let discard p =
    p >>= fun _ -> return ()

let rec many1 p =
    p >>= fun (r, _) -> (many1 p >>= fun (rs, _) -> return (r :: rs)) <|> return [r]

let many p =
    many1 p <|> return []

let sepby1 p s =
    p <~> many (s >> p)

let sepby p s =
    sepby1 p s <|> return []

let chainl1 p s =
    let rec next (lhs, idx) =
        (s >>= fun (op, _) -> p >>= fun (rhs, _) -> next ((op lhs rhs), idx)) <|> return lhs
    in
    p >>= next

let token inp =
    let (chr, idx) = inp in
    match chr with
    | [] -> Error (Err.Eof idx)
    | first :: rest -> Ok ((first, idx), (rest, idx + 1))

let chr chr =
    token <?> fun c -> c = chr

let not chr =
    token <?> fun c -> c != chr

let str str =
    let rec make chrs =
        match chrs with
        | [] -> return []
        | first :: rest -> chr first >>= fun (c, _) -> make rest >>= fun (cs, _) -> return (c :: cs)
    in
    chrs_of_str str |> make => str_of_chrs

let digit = token <?> fun c -> match c with '0'..'9' -> true | _ -> false
let lowercase = token <?> fun c -> match c with 'a'..'z' -> true | _ -> false
let uppercase = token <?> fun c -> match c with 'A'..'Z' -> true | _ -> false
let letter = lowercase <|> uppercase
let alphanum = letter <|> digit
