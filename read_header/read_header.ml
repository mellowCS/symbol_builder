open Core_kernel
open Bap.Std

(** Refers to information bundled with application*)
include Self()


type func_sym = {
  address: tid;
  name : string;
  cconv : string;
  args : (string * string) list
}


let remove_sub_name (sub : string) (arg : string) : string =
  String.chop_prefix_exn arg ~prefix:(sub^"_")


let print_symbol (sym : func_sym) =
  print_endline (Printf.sprintf "Function: %s" sym.name);
  print_endline (Printf.sprintf "  Address: %s" (Tid.to_string sym.address));
  if List.is_empty sym.args = false then
    begin
      print_endline "  Args:";
      List.iter ~f:(fun (var, intent) ->
          print_endline (Printf.sprintf "    arg: %s, intent: %s" var intent)
        ) sym.args
    end;
  print_endline (Printf.sprintf "  Calling Convetion: %s\n" sym.cconv)


let print_sub_attrs (program : program term) =
  Term.enum sub_t program |>
  Seq.iter ~f:(fun s ->
      let name = Sub.name s in
      let vars = ref [||] in
      Term.enum arg_t s |>
      Seq.iter ~f:(fun a ->
          let intention = match Arg.intent a with
            | Some(In) -> "IN"
            | Some(Out) -> "OUT"
            | Some(Both) -> "INOUT"
            | None -> "UNKNOWN" in
          vars := Array.append !vars [|(remove_sub_name name (Exp.to_string (Bil.Var(Arg.lhs a))), intention)|]
      );
      let fun_symbol = {address = (Term.tid s); name = name; cconv = "cdecl"; args = (Array.to_list !vars);} in
      print_symbol fun_symbol
  )


let main project =
  let prog = Project.program project in
  print_sub_attrs prog


let () =
  Project.register_pass' ~deps:["api"] (main)
