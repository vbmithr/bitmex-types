open Core

module BitmexType = struct
  type t =
    | Guid
    | Boolean
    | Integer
    | Long
    | Float
    | String
    | Timestamp
    | Timespan
    | Any
    | UserPreferences

  let of_string s =
    match String.lowercase s with
    | "guid" -> Guid
    | "boolean" -> Boolean
    | "integer" -> Integer
    | "long" -> Long
    | "float" -> Float
    | "string" | "symbols" | "text" -> String
    | "timestamp" -> Timestamp
    | "timespan" -> Timespan
    | "any" | "object" -> Any
    | "userpreferences" -> UserPreferences
    | _ -> invalid_arg ("Bitmex.of_string: got " ^ s)

  let to_type = function
    | Guid -> "Uuid.Unstable.t"
    | Boolean -> "bool"
    | Integer -> "int"
    | Long -> "int"
    | Float -> "float"
    | String -> "string"
    | Timestamp -> "Time_ns.t"
    | Timespan -> "Time_ns.Span.t"
    | Any -> "Yojson.Safe.json"
    | UserPreferences -> "UserPreferences.t"
end

let extract_member = function
  | BitmexType.Guid -> "to_string |> Uuid.of_string"
  | Boolean         -> "to_bool"
  | Integer         -> "to_int"
  | Long            -> "to_int"
  | Float           -> "fun v -> try to_float v with Type_error _ -> Float.of_int (to_int v)"
  | String          -> "to_string"
  | Timestamp       -> "to_string |> Time_ns.of_string"
  | Timespan        -> "to_string |> Time_ns.Span.of_string"
  | Any             -> "Fn.id"
  | UserPreferences -> "UserPreferences.of_yojson"

exception UnsupportedType

let protect_key = function
  | "type" -> "typ"
  | "open" -> "opn"
  | s -> s

let gen_module name json =
  let open Printf in
  let open Yojson.Safe in
  let buf = Buffer.create 319 in
  let keys =
    Util.(member "keys" json |> to_list) |>
    List.map ~f:Util.to_string in
  let types =
    Util.(member "types" json |> to_assoc) |>
    List.map ~f:begin fun (name, typ) ->
      match typ with
      | `String s -> name, (true, BitmexType.of_string s)
      | `List l -> name, (false, Util.(to_string (List.hd_exn l)) |> BitmexType.of_string)
      | #Yojson.Safe.json -> raise UnsupportedType
    end in
  if List.(length keys = 0 && length types = 0) then raise UnsupportedType ;
  Buffer.add_string buf (sprintf "module %s = struct\n  type t = {\n" name ) ;
  let contains_any = List.fold_left ~init:false types ~f:begin fun a (k, (atom, t)) ->
    let v = match atom, List.mem ~equal:String.equal keys k with
      | true, true -> BitmexType.to_type t
      | true, false -> BitmexType.to_type t ^ " option"
      | _ -> BitmexType.to_type t ^ " list"
    in
    Buffer.add_string buf (sprintf "    %s : %s ;\n"
                             (String.uncapitalize k |> protect_key) v) ;
    if t = Any || t = UserPreferences then true else a || false
  end in
  Buffer.add_string buf (sprintf "  }%s\n\n" (if contains_any then "" else " [@@deriving sexp]"));
  Buffer.add_string buf
{|  let of_yojson json = Yojson.Safe.Util.{
|} ;
  List.iter types ~f:begin fun (k, (atom, t)) ->
    let k' = (String.uncapitalize k |> protect_key) in
    if List.mem ~equal:String.equal keys k then
      Buffer.add_string buf (sprintf "    %s = member \"%s\" json |> %s ;\n" k' k (extract_member t))
    else if not atom then
      Buffer.add_string buf (sprintf "    %s = member \"%s\" json |> to_list |> List.map ~f:%s ;\n"
                               k' k (extract_member t))
    else
      Buffer.add_string buf (sprintf "    %s = (try Some (member \"%s\" json |> %s) with _ -> None) ;\n"
                               k' k (extract_member t))
  end ;
  Buffer.add_string buf "  }\n\n" ;
  Buffer.add_string buf
{|  let merge t t' = {
|} ;
  List.iter types ~f:begin fun (k, (atom, _)) ->
    let k' = (String.uncapitalize k |> protect_key) in
    if List.mem ~equal:String.equal keys k then
      Buffer.add_string buf (sprintf "    %s = t'.%s ;\n" k' k')
    else if not atom then
      Buffer.add_string buf (sprintf "    %s = (match t.%s, t'.%s with (_, []) -> t.%s | _ -> t'.%s) ;\n"
                               k' k' k' k' k')
    else
      Buffer.add_string buf
        (sprintf "    %s = Option.first_some t'.%s t.%s ;\n" k' k' k')
  end ;
  Buffer.add_string buf "  }\n" ;
  Buffer.add_string buf "end" ;
  Buffer.contents buf

let () =
  let open Yojson.Safe in
  let schema = Stdio.In_channel.read_all Sys.argv.(1) |> from_string in
  let modules = Util.keys schema in
  let modules =
    List.group modules ~break:String.equal in
  Stdio.Out_channel.with_file (Sys.argv.(2))
    ~append:false ~binary:false ~fail_if_exists:false ~f:begin fun oc ->
    Stdio.Out_channel.fprintf oc "open Core\n\n" ;
    List.iter modules ~f:begin fun group ->
      List.iter (List.rev group) ~f:begin fun name ->
        begin try
            Stdio.Out_channel.fprintf oc "%s" (gen_module name (Util.member name schema)) ;
            Stdio.Out_channel.fprintf oc "\n\n"
          with
            UnsupportedType -> () ;
        end ;
      end
    end
  end
