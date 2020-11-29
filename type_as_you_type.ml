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
    Some (start, end_)
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
      match previous_expr () with
      | None -> ()
      | Some (start, end_) ->
        Background.don't_wait_for [%here] (fun () ->
            let buffer = Current_buffer.get () in
            let%map type_ = Merlin.type_region ~buffer ~start ~end_ in
            Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
                match String.chop_suffix type_ ~suffix:" Deferred.t" with
                | None -> ()
                | Some prefix ->
                  let point = Point.marker_at () in
                  let insert_before_markers =
                    Funcall.("insert-before-markers" <: string @-> return nil)
                  in
                  Point.goto_char end_;
                  Point.delete_forward_char_exn
                    (Position.diff
                       (Marker.position point |> Option.value_exn)
                       (Point.get ()));
                  insert_before_markers " in";
                  Point.goto_char start;
                  Point.insert
                    (match prefix with
                    | "unit" -> "let%bind () = "
                    | _ -> "let%bind (_ : " ^ prefix ^ ") = ");
                  Point.goto_char (point |> Marker.position |> Option.value_exn);
                  ())))
;;

let () = provide ("type_as_you_type" |> Symbol.intern)
