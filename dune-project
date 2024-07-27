(lang dune 3.16)

(name zeus)

(generate_opam_files true)

(source
 (github jaredborders/zeus))

(authors "Jared Borders")

(maintainers "Jared Borders")

(license MIT)

(documentation https://github.com/jaredborders/zeus/docs/)

(package
 (name zeus)
 (synopsis "A functional limit orderbook (LOB) implementation in OCaml")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  core
  ppx_jane
  ppx_inline_test)
 (tags ("orderbook")))
