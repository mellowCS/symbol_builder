open Core_kernel
open Bap.Std
open Graphlib.Std

(** Refers to information bundled with application*)
include Self()

let dyn_syms = ref None

type func_sym = {
  tid: tid;
  start_address : Addr.t;
  end_address : Addr.t;
  name : string;
  cconv : string option;
  args : (Var.t * Exp.t * intent option) list
}


let remove_sub_name (sub : string) (arg : string) : string =
  String.chop_prefix_exn arg ~prefix:(sub^"_")


let parse_dyn_sym_line (line : string) : string option =
  let line = ref (String.strip line) in
  let str_list = ref [] in
  while Option.is_some (String.rsplit2 !line ~on:' ') do
    let (left, right) = Option.value_exn (String.rsplit2 !line ~on:' ') in
    line := String.strip left;
    str_list := right :: !str_list;
  done;
  str_list := !line :: !str_list;
  match !str_list with
  | value :: func1 :: func2 :: _ -> begin
      match ( String.strip ~drop:(fun x -> x = '0') value ) with
      | "" -> begin
          if (String.equal func1 "DF" || String.equal func2 "DF") then (
            List.last !str_list
          )
          else None
        end
      | _ -> None (* The symbol has a nonzero value, so we assume that it is not an extern function symbol. *)
    end
  | _ -> None


let parse_dyn_syms (project : Project.t) : String.Set.t =
  match !dyn_syms with
  | Some(symbol_set) -> symbol_set
  | None ->
    match Project.get project filename with
    | None -> failwith "[CWE-checker] Project has no file name."
    | Some(fname) -> begin
        let cmd = Format.sprintf "objdump --dynamic-syms %s" fname in
        try
          let in_chan = Unix.open_process_in cmd in
          let lines = In_channel.input_lines in_chan in
          let () = In_channel.close in_chan in begin
            match lines with
            | _ :: _ :: _ :: _ :: tail -> (* The first four lines are not part of the table *)
              let symbol_set = String.Set.of_list (List.filter_map tail ~f:parse_dyn_sym_line) in
              dyn_syms := Some(symbol_set);
              symbol_set
            | _ ->
              dyn_syms := Some(String.Set.empty);
              String.Set.empty              (*  *)
          end
        with
          Unix.Unix_error (e,fm,argm) ->
          failwith (Format.sprintf "[CWE-checker] Parsing of dynamic symbols failed: %s %s %s" (Unix.error_message e) fm argm)
      end


let get_project_calling_convention (project : Project.t) : string option =
  Project.get project Bap_abi.name


let intent_to_string (intent : intent option) : string =
  match intent with
  | Some(In) -> "In"
  | Some(Out) -> "Out"
  | Some(Both) -> "InOut"
  | None -> "Unknown"


let get_start_end_address (project : Project.t) (extern_symbols : string list) =
  let syms = Project.symbols project in
  let sym_addr_tbl = Hashtbl.create (module String) ~size:(List.length extern_symbols) in
  Seq.iter (Symtab.to_sequence syms) ~f:(fun (name, entry, cfg) ->
      if Stdlib.List.mem name extern_symbols then
        begin
          let blks = Seq.to_list (Graphlib.reverse_postorder_traverse (module Graphs.Cfg) ~start:entry cfg) in
          let first = Memory.min_addr (Block.memory (List.hd_exn blks)) in
          let last = Memory.max_addr (Block.memory (List.last_exn blks)) in
          Hashtbl.add_multi sym_addr_tbl ~key:name ~data:first;
          Hashtbl.add_multi sym_addr_tbl ~key:name ~data:last;
        end
      else ()
    );
  sym_addr_tbl



let build_fun_symbols (project : Project.t) (program : program term) : func_sym list =
  let extern_symbols = String.Set.to_list (parse_dyn_syms project) in
  let sym_addr_tbl = get_start_end_address project extern_symbols in
  let calling_convention = get_project_calling_convention project in
  Seq.to_list (Seq.filter_map (Term.enum sub_t program) ~f:(fun s ->
      let sub_name = Sub.name s in
      if (Stdlib.List.mem sub_name extern_symbols) then
        begin
          let start_end = Hashtbl.find_multi sym_addr_tbl sub_name in
          let args = Seq.to_list (Seq.map (Term.enum arg_t s) ~f:(fun a ->
              (Arg.lhs a, Arg.rhs a, Arg.intent a)
            )) in
          Some({tid=(Term.tid s); start_address=(List.last_exn start_end); end_address=(List.hd_exn start_end); name=sub_name; cconv=calling_convention; args=args;})
        end
      else None
    ))


let print_function_symbols (syms : func_sym list) : unit =
  List.iter syms ~f:(fun sym ->
      let cconv = match sym.cconv with
        | Some(conv) -> conv
        | _ -> "" in
      print_endline (Printf.sprintf "Function: %s\n  tid: %s\n  start addr: %s\n  end addr: %s\n  cconv: %s" sym.name (Tid.name sym.tid) (Addr.to_string sym.start_address) (Addr.to_string sym.end_address) cconv);
      if List.is_empty sym.args <> true then print_endline "  args:";
      List.iter sym.args ~f:(fun arg ->
          let (var, exp, intent) = match arg with
            | (v, e, i) -> (remove_sub_name sym.name (Var.name v), (Exp.to_string e), (intent_to_string i))
            | _ -> failwith "Something went wrong." in
          print_endline (Printf.sprintf "    # var: %s ; exp: %s ; intent: %s" var exp intent)
        );
      print_endline ""
    )


let main (project : Project.t) : unit =
  let prog = Project.program project in
  let function_symbols = build_fun_symbols project prog in
  print_function_symbols function_symbols


let () =
  Project.register_pass' (main)
