(lang dune 3.22)

(name gui)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  core
  core_unix
  raylib
  raygui
  spawn
  (ocaml-lsp-server :dev)
  (merlin :dev)
  (ocamlformat :dev)
  (ppx_deriving :build))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
