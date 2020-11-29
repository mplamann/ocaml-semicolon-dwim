open! Core
open! Async
open! Ecaml

let () =
  Type_as_you_type_lib.Type_as_you_type.initialize ();
  provide ("ecaml_plugin" |> Symbol.intern)
;;
