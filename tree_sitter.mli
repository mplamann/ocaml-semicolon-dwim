(** Functions for interacting with emacs-tree-sitter, an incremental parsing library. *)

open! Core
open! Async
open! Import

module Node : sig
  module Type : sig
    type t =
      | Anonymous of string
      | Named of Symbol.t
    [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  val type_ : t -> Type.t
  val at_point : unit -> t
  val get_prev_sibling : t -> t
  val start_byte : t -> Position.t
  val end_byte : t -> Position.t
end
