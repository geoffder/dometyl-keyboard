(lang dune 3.5)

(using directory-targets 0.1)

(name dometyl)

(generate_opam_files true)

(source
 (github geoffder/dometyl-keyboard))

(license AGPL-3.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(package
 (name dometyl)
 (synopsis "Ergonomic keyboard generator")
 (description
  "DSL to generate OpenSCAD models of concave split ergonomic keyboards.")
 (depends
  (ocaml
   (>= 4.08.0))
  (OCADml
   (>= 0.1.0))
  (OSCADml
   (>= 0.1.0))
  (ppx_deriving_cad
   (>= 0.1.0))))
