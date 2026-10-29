(lang dune 3.24)

(name balatro)

(generate_opam_files true)

(source
 (github alinespereira/balatro))

(authors "Aline <line.asramos@gmail.com>")

(maintainers "Aline <line.asramos@gmail.com>")

(package
 (name balatro)
 (synopsis "An OCaml model of Balatro cards and scoring rules")
 (description
  "Work-in-progress OCaml project modeling Balatro cards, decks, poker hands, consumables, and scoring effects.")
 (depends ocaml ppx_deriving ppx_enumerate)
 (tags
  ("balatro" "card-game" "poker")))
