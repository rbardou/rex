(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/rex))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name rex)
 (synopsis "Regular Expression - eXtended")
 (description "This module provide a wrapper around the re library with a simpler interface. Infix operators inspired by Perl are provided. Regular expressions are annotated with their type.")
 (depends (ocaml (>= 4.07)) (re (>= 1.9.0))))
