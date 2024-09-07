(lang dune 3.16)

(name reading_list)

(generate_opam_files true)

(source
 (github liate7/reading-list-db))

(authors "Andrew Patterson")

(maintainers "Andrew Patterson")

(license MIT)

(package
 (name reading_list)
 (synopsis "A web app for maintaining my reading list")
 (description "")
 (depends ocaml dune
          containers
          lwt dream dream-html cohttp cohttp-lwt
          caqti caqti-lwt caqti-driver-sqlite3
          yojson ppx_yojson_conv
          timedesc
          ppx_string ppx_compare ppx_blob
          (ocamlformat :with-dev-setup) (merlin :with-dev-setup) (utop :with-dev-setup)
          (ocaml-print-intf :with-dev-setup) (odig :with-dev-setup)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
