open! Core
open! Async
module Async_process = Process
open! Import
module Process = Async_process

let merlin_command = Funcall.("merlin-command" <: nullary @-> return string)

let type_region ~buffer ~start ~end_ =
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
  let%map result = Process.run_exn ~prog:(merlin_command ()) ~args:command ~stdin () in
  let read_from_string = Funcall.("read-from-string" <: string @-> return value) in
  let assoc = Funcall.("assoc" <: Symbol.t @-> value @-> return value) in
  let read = read_from_string result in
  let read = Value.car_exn read in
  assoc (Symbol.intern "value") read |> Value.cdr_exn |> Value.to_utf8_bytes_exn
;;
