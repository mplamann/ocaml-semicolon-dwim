open! Core
open! Async
open! Import

let minor_mode =
  Minor_mode.define_minor_mode
    ("type-as-you-type-mode" |> Symbol.intern)
    [%here]
    ~docstring:"Minor mode for automatically inserting let%bind as you type"
    ~mode_line:"tt"
    ~global:false
    ()
;;

let keymap = Minor_mode.keymap_exn minor_mode

let previous_expr () =
  let node_at_point =
    Current_buffer.save_excursion Sync (fun () ->
        Point.backward_char_exn 1;
        Tree_sitter.Node.at_point ())
  in
  match Tree_sitter.Node.type_ node_at_point with
  | Anonymous ";" ->
    let expr_node = Tree_sitter.Node.get_prev_sibling node_at_point in
    let start = Tree_sitter.Node.start_byte expr_node in
    let end_ = Tree_sitter.Node.end_byte expr_node in
    Some (Current_buffer.contents ~start ~end_ ())
  | _ -> None
;;

let () =
  defun_nullary_nil
    ("type-as-you-type-semicolon" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    ~define_keys:[ keymap, ";" ]
    (fun () ->
      Point.insert ";";
      message_s [%sexp [%here]];
      match previous_expr () with
      | None -> ()
      | Some expr -> message_s [%sexp (expr : Text.t)])
;;

let () = provide ("type_as_you_type" |> Symbol.intern)
