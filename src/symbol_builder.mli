open Bap.Std
open Core_kernel

type func_sym = {
  tid: tid;
  start_address : Bap.Std.Addr.t;
  end_address : Bap.Std.Addr.t;
  name : string;
  cconv : string option;
  args : (Bap.Std.Var.t * Bap.Std.Exp.t * Bap.Std.intent option) list
}

val parse_dyn_sym_line : string -> string option

val parse_dyn_syms : Bap.Std.Project.t -> String.Set.t

val get_project_calling_convention : Bap.Std.Project.t -> string option

val build_fun_symbols : Bap.Std.Project.t -> Bap.Std.program Bap.Std.term -> func_sym list

val print_function_symbols : func_sym list -> unit

val main : Bap.Std.Project.t -> unit
