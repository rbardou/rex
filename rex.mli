(** Simple interface for regular expressions. *)

(** {2 Capture Groups} *)

(** Capture group specifications.

    Used when defining regular expressions to specify the type of
    regular expressions, i.e. what [find] returns on a match.

    Usually you would use:
    - [_0] when you just want to test for presence;
    - [full] when you want to get the full match;
    - [_1], [_2], ... when you want to extract sub-parts of the regular expression;
    - [full * _1], [full * _2], ... when you want both the full match
      and some sub-parts.

    Those specifications are functions that convert a [Group.t]
    into useful values. Those values are usually strings but you
    can also include some conversions such as [int_of_string_opt].

    The capture group specifications below ([_0], [_1], etc. and [full])
    return the empty string [""] for groups which were not captured. *)
type 'a groups = Re.Group.t -> 'a

(** Use when defining a regular expression with 0 capture groups.

    Matching with [=~] returns [Some ()]; usually one uses [=~?] or [=~!] instead. *)
val _0: unit groups

(** Use when defining a regular expression with 1 capture group. *)
val _1: string groups

(** Use when defining a regular expression with 2 capture groups. *)
val _2: (string * string) groups

(** Use when defining a regular expression with 3 capture groups. *)
val _3: (string * string * string) groups

(** Use when defining a regular expression with 4 capture groups. *)
val _4: (string * string * string * string) groups

(** Use to retrieve the full match, i.e. capture group 0. *)
val full: string groups

(** Carthesian product of two capture group specifications.

    Usually, this is only used to retrieve both the full match
    and capture groups (example: [full * _2]). *)
val ( * ): 'a groups -> 'b groups -> ('a * 'b) groups

(** {2 Constructors} *)

(** Typed regular expressions.

    ['a] is the type that is returned by [find] in case of a match;
    it is specified with a capture group specification (type [groups]). *)
type 'a t

(** Convert a [Core.re] regular expression into one that can be used with this module. *)
val re: 'a groups -> Re.re -> 'a t

(** Compile a regular expression expressed using Emacs regular expression syntax. *)
val emacs: ?case: bool -> 'a groups -> string -> 'a t

(** Compile a regular expression expressed using Shell glob syntax. *)
val glob:
  ?anchored:bool ->
  ?pathname:bool ->
  ?period:bool ->
  ?expand_braces:bool ->
  'a groups -> string -> 'a t

(** Compile a regular expression expressed using Perl regular expression syntax. *)
val perl: ?opts: Re.Perl.opt list -> 'a groups -> string -> 'a t

(** Compile a regular expression expressed using PCRE syntax. *)
val pcre: ?flags: Re.Pcre.flag list -> 'a groups -> string -> 'a t

(** Compile a regular expression expressed using Posix extended regular expression syntax. *)
val posix: ?opts: (Re.Posix.opt list) -> 'a groups -> string -> 'a t

(** {2 Matching} *)

(** Some of the functions below have infix operator versions.
    Those infix operators do not support [?pos] and [?len] arguments,
    and the remaining arguments are in reverse order.
    For instance, [s =~ r] is the same as [find r s],
    which can also be written [s |> find r]
    (hence the different argument order). *)

(** Also note that contrary to the examples below, in practice you may
    want to define your regular expression at toplevel, such as:
    [let int_re = Rex.(perl full "\\d+")]
    so that the regular expression is compiled only once, for efficiency's sake.
    This is only important if your regular expression is used several times. *)

(** Test whether a string matches a regular expression and return captured values. *)
val find: ?pos: int -> ?len: int -> 'a t -> string -> 'a option

(** Same as [find].

    Example:
    [
      match Rex.(s =~ perl _2 "(\\d+) and (\\d+)") with
        | None -> ...
        | Some (left, right) -> ...
    ]

    The operator name [=~] comes from Perl. *)
val (=~): string -> 'a t -> 'a option

(** Same as [find] but return all occurrences. *)
val find_all: ?pos: int -> ?len: int -> 'a t -> string -> 'a list

(** Same as [find_all].

    Example: [let integers = Rex.(s =~* perl full "\\d+")] *)
val (=~*): string -> 'a t -> 'a list

(** Test whether a string matches a regular expression. *)
val matches: ?pos: int -> ?len: int -> _ t -> string -> bool

(** Same as [matches].

    Example: [if Rex.(s =~? perl _0 "\\d+") then print_endline "found an integer"] *)
val (=~?): string -> _ t -> bool

(** Negation of [=~?].

    Example: [if Rex.(s =~! perl _0 "\\d+") then print_endline "found no integer"] *)
val (=~!): string -> _ t -> bool

(** Split a string at occurrences of a regular expression.

    Return [(items, remainder)] where [items] is a list
    [[(text1, delimiter1); ...; (textN, delimiterN)]]
    such that concatenating [[text1; delimiter1; ...; textN; delimiterN; remainder]]
    would return the original string if delimiters had capture group specification [full].
    The regular expression can no longer be found in [text1, ..., textN],
    nor in [remainder]. *)
val split:
  ?pos: int ->
  ?len: int ->
  'a t -> string -> (string * 'a) list * string

(** Same as [split]. *)
val (=~/): string -> 'a t -> (string * 'a) list * string

(** Replace occurrences of a regular expression in a string.

    Usage: [replace rex s substitution]

    Function [substitution] is given occurrences of the regular expression.
    It shall return the substring to replace those occurrences with.

    If [all] is [false], replace only the first occurrence.
    If [all] is [true], replace all occurrences.
    Default is [true].

    Example:
    [
      let increment_all_integers s =
        Rex.(replace (perl full "\\d+")) s @@ fun i ->
        int_of_string_opt i |> Option.value ~default: 0 |> (+) 1 |> string_of_int
    ] *)
val replace:
  ?pos: int ->
  ?len: int ->
  ?all: bool ->
  'a t -> string -> ('a -> string) -> string
