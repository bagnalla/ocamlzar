(lang dune 3.7)

(name zar)

(generate_opam_files true)

(source
 (github bagnalla/ocamlzar))

(authors "Alexander Bagnall")

(maintainers "Alexander Bagnall <abagnalla@gmail.com>")

(license MIT)

(package
 (name zar)
 (version 0.9.3)
 (synopsis "Formally verified sampling from discrete probability distributions")
 (description "See the [paper](https://arxiv.org/abs/2211.06747) (to appear in PLDI'23) and [Github repository](https://github.com/bagnalla/zar).")
 (depends (ocaml (>= 4.08)) dune (alcotest :with-test) (qcheck :with-test) (qcheck-alcotest :with-test))
 (tags
  (zar random sampler verified coq discrete)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
