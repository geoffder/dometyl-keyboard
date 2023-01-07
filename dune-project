(lang dune 3.5)

(using directory-targets 0.1)

(using dune_site 0.1)

(name dometyl)

(generate_opam_files true)

(source
 (github geoffder/dometyl-keyboard))

(license AGPL-3.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(package
 (name dometyl)
 (sites
  (share imports))
 (synopsis "Ergonomic keyboard generator")
 (description
  "DSL to generate OpenSCAD models of concave split ergonomic keyboards.")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  dune-site
  (OCADml
   (>= 0.2.2))
  (OSCADml
   (>= 0.1.1))
  (ppx_deriving_cad
   (>= 0.1.0))))
