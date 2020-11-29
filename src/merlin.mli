open! Core
open! Async
open! Import

val type_region
  :  buffer:Buffer.t
  -> start:Position.t
  -> end_:Position.t
  -> string Deferred.t
