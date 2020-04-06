open Core_kernel
open Bap.Std

(** Refers to information bundled with application*)
include Self()


type func_sym = {
  address: tid;
  name : string;
  cconv : string;
  args : (string * string * string) list
}


let remove_sub_name (sub : string) (arg : string) : string =
  String.chop_prefix_exn arg ~prefix:(sub^"_")


let print_symbol (sym : func_sym) : unit =
  print_endline (Printf.sprintf "Function: %s" sym.name);
  print_endline (Printf.sprintf "  Address: %s" (Tid.to_string sym.address));
  if List.is_empty sym.args = false then
    begin
      print_endline "  Args:";
      List.iter ~f:(fun (var, exp, intent) ->
          print_endline (Printf.sprintf "    arg: %s, loc: %s, intent: %s" var exp intent)
        ) sym.args
    end;
  print_endline (Printf.sprintf "  Calling Convention: %s\n" sym.cconv)


let get_stack_pos (exp : string) : string =
  let sub_list = String.split_on_chars exp ~on:['['; ']'] in
  match sub_list with
  | _::pos::_ -> begin
      match String.split pos ~on:',' with
      | hd::_ -> hd
      | _ -> failwith "Something went wrong while extracting the stack position @ read_header.ml"
    end
  | _ ->  failwith "Something went wrong while extracting the stack position @ read_header.ml"


let print_sub_attrs (program : program term) : unit =
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
          let var_name = (remove_sub_name name (Exp.to_string (Bil.Var(Arg.lhs a)))) in
          let expression = Exp.to_string (Arg.rhs a) in
          let expression =
            match String.is_substring expression ~substring:"ESP +" with
            | true -> get_stack_pos expression
            | false -> expression in
          vars := Array.append !vars [|(var_name, expression, intention)|]
      );
      let fun_symbol = {address = (Term.tid s); name = name; cconv = "cdecl"; args = (Array.to_list !vars);} in
      print_symbol fun_symbol
  )


let main (project : project) : unit =
  let prog = Project.program project in
  print_sub_attrs prog


let () =
  Project.register_pass' ~deps:["api"] (main)
