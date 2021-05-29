type 'a groups = Re.Group.t -> 'a

let _0 _ = ()

let get g n = try Re.Group.get g n with Not_found -> ""

let _1 g = get g 1

let _2 g = get g 1, get g 2

let _3 g = get g 1, get g 2, get g 3

let _4 g = get g 1, get g 2, get g 3, get g 4

let full g = get g 0

let ( * ) a b g = a g, b g

type 'a t =
  {
    re: Re.re;
    groups: 'a groups;
  }

let re groups re = { re; groups }

let emacs ?case groups s =
  { re = Re.Emacs.compile (Re.Emacs.re ?case s); groups }

let glob ?anchored ?pathname ?period ?expand_braces groups s =
  {
    re = Re.compile (Re.Glob.glob ?anchored ?pathname ?period ?expand_braces s);
    groups;
  }

let perl ?opts groups s =
  { re = Re.Perl.compile (Re.Perl.re ?opts s); groups }

let pcre ?flags groups s =
  { re = Re.compile (Re.Pcre.re ?flags s); groups }

let posix ?opts groups s =
  { re = Re.Posix.compile (Re.Posix.re ?opts s); groups }

let find ?pos ?len { re; groups } s =
  match Re.exec_opt ?pos ?len re s with
    | None ->
        None
    | Some g ->
        Some (groups g)

let (=~) s rex = find rex s

let find_all ?pos ?len { re; groups } s =
  List.map groups (Re.all ?pos ?len re s)

let (=~*) s rex = find_all rex s

let matches ?pos ?len { re; groups = _ } s =
  Re.execp ?pos ?len re s

let (=~?) s rex = matches rex s

let (=~!) s rex = not (matches rex s)

let split ?pos ?len { re; groups } s =
  let rec gather ?(acc_text = []) ?(acc = []) = function
    | [] ->
        List.rev acc, String.concat "" (List.rev acc_text)
    | `Text s :: tl ->
        gather ~acc_text: (s :: acc_text) ~acc tl
    | `Delim g :: tl ->
        let text = String.concat "" (List.rev acc_text) in
        gather ~acc: ((text, groups g) :: acc) tl
  in
  gather (Re.split_full ?pos ?len re s)

let (=~/) s rex = split rex s

let replace ?pos ?len ?all { re; groups } s f =
  Re.replace ?pos ?len ?all re ~f: (fun g -> f (groups g)) s
