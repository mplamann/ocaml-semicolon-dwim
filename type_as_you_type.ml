open! Core
open! Async
open! Ecaml

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

let () =
  defun_nullary_nil
    ("type-as-you-type-semicolon" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    ~define_keys:[ keymap, ";" ]
    (let regexp = Regexp.of_pattern "foo" in
     fun () ->
       Point.insert ";";
       let point = Point.marker_at () in
       Current_buffer.save_excursion Sync (fun () ->
           if Point.search_backward_regexp regexp
              && Point.looking_at (Regexp.of_pattern "foo;")
           then (
             Point.delete_forward_char_exn 3;
             Point.insert "bar";
             Marker.position point |> Option.iter ~f:Point.goto_char)))
;;

let () = provide ("type_as_you_type" |> Symbol.intern)
