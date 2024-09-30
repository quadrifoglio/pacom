(** Parser error type interface. *)
module Err : sig
    type t =
        | Eof of int
          (** Enf of the input. *)
        | Message of int * string
          (** An error with a textual description. *)
        | Nested of int * t
          (** An error that was triggered because of a failure inside of a nested parser. *)

    val pos : t -> int
    (** [pos] returns the character index at which the error occurred. *)
end

(** Parser input type interface. *)
module Input : sig
    type t = (char list * int)
    (** The type of what is feeded into parsers: the list and index of
        unconsumed characters. *)

    val init : string -> t
    (** [init] creates a parser input from the specified string. *)

    val pos : t -> int
    (** [pos] returns the input's character index position. *)
end

(** Parser type interface. *)
module Parser : sig
    type 'a t = Input.t -> (('a * int) * Input.t, Err.t) result
    (** Parser for objects of type ['a]. It is a function that tries to parse
        an object from the specified input. In case it succeeds, it returns the
        parsed ['a] object and the character index at which the object was parsed,
        as well as the unconsumed input. In case of failure it returns an [Err.t]
        object. *)
end

val fail : string -> 'a Parser.t
(** [fail msg] returns a parser that always fails with the specified error
    message. *)

val return : 'a -> 'a Parser.t
(** [return a] returns a parser that always succeeds and returns the specified
    [a] value.  *)

val ( <|> ) : 'a Parser.t -> 'a Parser.t -> 'a Parser.t
(** [a <|> b] returns a parser that first applies [a], and in case [a] fails
    without consuming any input, tries to apply [b] (or combinator). *)

val ( >>= ) : 'a Parser.t -> (('a * int) -> 'b Parser.t) -> 'b Parser.t
(** Monadic bind operator. *)

val ( let* ) : 'a Parser.t -> (('a * int) -> 'b Parser.t) -> 'b Parser.t
(** Monadic bind operator in let binding form. *)

val ( <?> ) : 'a Parser.t -> ('a -> bool) -> 'a Parser.t
(** [a <?> f] returns a parser that will only succeed in case [a] succeeds and
    the parsed value also satisfies predicate [f]. *)

val ( => ) : 'a Parser.t -> ('a -> 'b) -> 'b Parser.t
(** [a => f] returns a parser that maps the ['a] value returned by [a] to ['b]
    using [f]. *)

val ( >> ) : 'a Parser.t -> 'b Parser.t -> 'b Parser.t
(** [a >> b] returns a parser that first applies [a], then applies [b], and
    returns the ['b] value produced by [b]. *)

val ( << ) : 'a Parser.t -> 'b Parser.t -> 'a Parser.t
(** [a << b] returns a parser that first applies [a], then applies [b], and
    returns the ['a] value produced by [a]. *)

val discard : 'a Parser.t -> unit Parser.t
(** [discard a] returns a parser that applies [a] and then discards its result,
    returining unit. *)

val many1 : 'a Parser.t -> 'a list Parser.t
(** [many1 a] returns a parser that applies [a] repeatedly until [a] fails,
    only succeeds in case [a] succeeded at least one time, and returns the list of
    parsed ['a] value. *)

val many : 'a Parser.t -> 'a list Parser.t
(** [many1 a] returns a parser that applies [a] repeatedly until [a] fails, and
    returns the list of parsed ['a] value. In case [a] did not succeed, an empty
    list is returned. *)

val sepby1 : 'a Parser.t -> 'b Parser.t -> 'a list Parser.t
(** [sepby1 a b] returns a parser that parses ['a] elements separated by ['b]
    elements, only succeeds in case at least one ['a] element was parsed, and
    returns the list of parsed ['a] elements. *)

val sepby : 'a Parser.t -> 'b Parser.t -> 'a list Parser.t
(** [sepby1 a b] returns a parser that parses ['a] elements separated by ['b]
    elements, and returns the list of parsed ['a] elements. If [a] did not succeed,
    an empty list is returned. *)

val chainl1 : 'a Parser.t -> ('a -> 'a -> 'a) Parser.t -> 'a Parser.t
(** [chainl1 a b] returns a parser that first applies [a] and the resulting
    value becomes the initial accumulator value. Then it parses [b a] sequences
    repeatedly and uses the function returned by [b] to combine the accumulator
    value with the value last returned by [a]. The result of that combination
    becomes the next accumulator value. *)

val token : char Parser.t
(** [token] returns a parser yielding a single character token from the input. *)

val chr : char -> char Parser.t
(** [chr c] returns a parser that only succeeds if the next input character
    token is [c], and returns [c]. *)

val not : char -> char Parser.t
(** [not c] returns a parser that only succeeds if the next input character
    token is not [c], and returns it. *)

val str : string -> string Parser.t
(** [str s] returns a parser that only succeeds if the next characters of the
    input correspond to string [s], and returns string [s]. *)

val digit : char Parser.t
(** [digit] returns a digit character parser ('0'..'9'). *)

val lowercase : char Parser.t
(** [lowercase] returns a lowercase character parser ('a'..'z'). *)

val uppercase : char Parser.t
(** [uppercase] returns an uppercase character parser ('A'..'Z'). *)

val letter : char Parser.t
(** [letter] returns a letter character parser. *)

val alphanum : char Parser.t
(** [alphanum] returns an alphanumeric character parser. *)
