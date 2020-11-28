open! Core
open! Async
module Async_process = Process
open! Import
module Process = Async_process

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
    Some (start, end_, Current_buffer.contents ~start ~end_ ())
  | _ -> None
;;

let merlin_type_region ~buffer ~start ~end_ =
  let command, stdin =
    Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
        ( [ "server"
          ; "type-expression"
          ; "-protocol"
          ; "sexp"
          ; "-log-file"
          ; "-"
          ; "-filename"
          ; Current_buffer.file_name_exn ()
          ; "-position"
          ; (let line, col =
               Current_buffer.save_excursion Sync (fun () ->
                   Point.goto_char end_;
                   Point.line_number (), Point.column_number ())
             in
             sprintf "%d:%d" line col)
          ; "-expression"
          ; Current_buffer.contents ~start ~end_ () |> Text.to_utf8_bytes
          ]
        , Current_buffer.contents () |> Text.to_utf8_bytes ))
  in
  (* message_s [%sexp { command : string list }]; *)
  let%map result =
    Process.run_exn
      ~prog:"/home/mplamann/.opam/default/bin/ocamlmerlin"
      ~args:command
      ~stdin
      ()
  in
  (* message result; *)
  let read_from_string = Funcall.("read-from-string" <: string @-> return value) in
  let assoc = Funcall.("assoc" <: Symbol.t @-> value @-> return value) in
  let read = read_from_string result in
  (* message_s [%sexp [%here], (read : Value.t)]; *)
  let read = Value.car_exn read in
  (* let listp = Funcall.("listp" <: value @-> return bool) in *)
  (* message_s [%sexp [%here], (read : Value.t), (listp read : bool)]; *)
  assoc (Symbol.intern "value") read |> Value.cdr_exn |> Value.to_utf8_bytes_exn
;;

let () =
  defun_nullary_nil
    ("type-as-you-type-semicolon" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    ~define_keys:[ keymap, ";" ]
    (fun () ->
      Point.insert ";";
      (* message_s [%sexp [%here]]; *)
      match previous_expr () with
      | None -> ()
      | Some (start, end_, _expr) ->
        (* message_s [%sexp (expr : Text.t)]; *)
        Background.don't_wait_for [%here] (fun () ->
            match%bind
              merlin_type_region ~buffer:(Current_buffer.get ()) ~start ~end_
            with
            | "unit Deferred.t" ->
              Point.goto_char end_;
              (* TODO: This doesn't work if there's whitespace between point and ; *)
              Point.delete_forward_char_exn 1;
              Point.insert " in";
              (* TODO: This isn't right -- this code is async. The point might have advanced already. *)
              let point = Point.marker_at () in
              Point.goto_char start;
              Point.insert "let%bind () = ";
              Point.goto_char (point |> Marker.position |> Option.value_exn);
              return ()
            | _ -> return ()))
;;

let () = provide ("type_as_you_type" |> Symbol.intern)
