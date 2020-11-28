open! Core
open! Async
open! Import

let () =
  Feature.require ("tree-sitter" |> Symbol.intern);
  Feature.require ("tree-sitter-langs" |> Symbol.intern)
;;

module Node = struct
  module Type = struct
    type t =
      | Anonymous of string
      | Named of Symbol.t
    [@@deriving sexp_of]

    let t =
      Value.Type.create
        [%sexp "Node.Type.t"]
        [%sexp_of: t]
        (fun value ->
          if Value.is_string value
          then Anonymous (Value.to_utf8_bytes_exn value)
          else Named (Symbol.of_value_exn value))
        (function
          | Anonymous string -> Value.of_utf8_bytes string
          | Named symbol -> Symbol.to_value symbol)
    ;;
  end

  type t = Value.t [@@deriving sexp_of]

  let t = Value.Type.value
  let type_ = Funcall.("tsc-node-type" <: t @-> return Type.t)
  let at_point = Funcall.("tree-sitter-node-at-point" <: nullary @-> return t)
  let get_prev_sibling = Funcall.("tsc-get-prev-sibling" <: t @-> return t)
  let start_byte = Funcall.("tsc-node-start-byte" <: t @-> return Position.t)
  let end_byte = Funcall.("tsc-node-end-byte" <: t @-> return Position.t)
end
