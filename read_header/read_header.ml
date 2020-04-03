open Core_kernel
open Bap.Std
open Format

(** Refers to information bundled with application*)
include Self()

(** Module Self
    - val name : name of plugin

    Module Self.Config
    - val param : 'a converter -> ... ?doc:string -> ... -> string -> 'a param
      --> converts string param into Bap type
    - val flag : ... -> ?doc:string -> string -> bool param
      --> sets bool parameter that is set to true if mentioned
    - val when_ready : (reader -> unit) -> unit
      --> requests system to call function once configuration params are established.
*)

type func_sym = {
  address: tid option;
  name : string;
  cconv : string;
  args : string list;
  ret : string
}


let cmdline_params = [
  ("header_dir", "Directory for C header files which can be included to improve information gathering of the disassembly");
  ("cconv", "Calling convention of the binary to be analysed")
]


(** Generate Bap params from string * string tuples of name and docstrings *)
let generate_bap_params params =
  List.map params (fun (name, docstring) -> (name, Config.param Config.string name ~doc:docstring))


(**let get_sub_params (program : program term) =*)


(** overwrite Term.visitor function enter_term. If Term has address add key value pair to address list*)
let generate_tid_map (prog : program term) : word Tid.Map.t =
  (object
    inherit [addr Tid.Map.t] Term.visitor
    method! enter_term _ t addrs = match Term.get_attr t address with
      | None -> addrs
      | Some addr -> Map.add_exn addrs ~key:(Term.tid t) ~data:addr
  end)#run prog Tid.Map.empty



let main params project =
  let cconv = String.Map.find_exn params "cconv" in
  let prog = Project.program project in
  let tid_map = generate_tid_map prog in


let () =
  let cmdline_params = generate_bap_params cmdline_params in
  let () = Config.when_ready (fun ({get=(!!)}) ->
      let params: String.t String.Map.t = List.fold cmdline_params ~init:String.Map.empty ~f:(fun param_map (name, bap_param) ->
          String.Map.set param_map ~key:name ~data:(!!bap_param)
        ) in
      Project.register_pass' ~deps:["callsites"; "x86"] (main params)
    ) in
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Tests the information improvement of function symbols by including calling convention and c header files"
    ] in
  ()
