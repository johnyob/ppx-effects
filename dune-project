(lang dune 2.9)

(generate_opam_files true)

(name ppx_effects)

(source
 (github johnyob/ppx-effects))

(license MIT)

(authors "Craig Ferguson, Alistair O'Brien")

(maintainers "ajob410@gmail.com")

(package
 (name ppx_effects)
 (synopsis "PPX syntax for untyped effects in OCaml 5.0")
 (description "PPX syntax for untyped effects in OCaml 5.0")
 (depends
  (ocaml
   (>= 5.0.0))
  (ppxlib
   (>= 0.12.0))
  (base
   (>= v0.15.1))))
