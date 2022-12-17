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
  (ocaml-variants
   (>= 5.0.0))
  (ppxlib
   (and
    (>= 0.12.0)))))
