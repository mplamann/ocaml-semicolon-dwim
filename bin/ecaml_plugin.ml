open! Core
open! Async
open! Ecaml

let () =
  (* This function call doesn't actually do anything, but it forces the ocaml
     compiler to actually link in Ocaml_semicolon_dwim_lib. Since the actual
     functionality provided by that library is entirely side-effects, that's all
     that's needed. *)
  Ocaml_semicolon_dwim_lib.Ocaml_semicolon_dwim.initialize ();
  provide ("ecaml_plugin" |> Symbol.intern)
;;
