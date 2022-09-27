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
    | AssetsConfigNetworkItem

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
    | "assetsconfignetworkitem" -> AssetsConfigNetworkItem
    | _ -> invalid_arg ("Bitmex.of_string: got " ^ s)
  ;;

  let to_type = function
    | Guid -> "Uuidm.t"
    | Boolean -> "bool"
    | Integer -> "int32"
    | Long -> "int64"
    | Float -> "float"
    | String -> "string"
    | Timestamp -> "Ptime.t"
    | Timespan -> "Ptime.Span.t"
    | Any -> "Yojson.Safe.t"
    | UserPreferences -> "UserPreferences.t"
    | AssetsConfigNetworkItem -> "AssetsConfigNetworkItem.t"
  ;;
end

let extract_member = function
  | BitmexType.Guid -> "to_string |> Uuidm.of_string |> Option.get"
  | Boolean -> "to_bool"
  | Integer -> "to_number |> Int32.of_float"
  | Long -> "to_number |> Int64.of_float"
  | Float -> "fun v -> try to_float v with Type_error _ -> Float.of_int (to_int v)"
  | String -> "to_string"
  | Timestamp -> "to_string |> ptime_of_string"
  | Timespan -> "to_string |> ptime_of_string |> Ptime.to_span"
  | Any -> "(fun a -> a)"
  | UserPreferences -> "UserPreferences.of_yojson"
  | AssetsConfigNetworkItem -> "AssetsConfigNetworkItem.of_yojson"
;;

exception UnsupportedType

let protect_key = function
  | "type" -> "typ"
  | "open" -> "opn"
  | s -> s
;;

let gen_module name json =
  let open Printf in
  let open Yojson.Safe in
  let buf = Buffer.create 319 in
  let keys = Util.(member "keys" json |> to_list) |> List.map ~f:Util.to_string in
  let types =
    Util.(member "types" json |> to_assoc)
    |> List.map ~f:(fun (name, typ) ->
         match typ with
         | `String s -> name, (true, BitmexType.of_string s)
         | `List l ->
           name, (false, Util.(to_string (List.hd_exn l)) |> BitmexType.of_string)
         | #Yojson.Safe.t -> raise UnsupportedType)
  in
  if List.(length keys = 0 && length types = 0) then raise UnsupportedType;
  Buffer.add_string buf (sprintf "module %s = struct\n  type t = {\n" name);
  let contains_any =
    List.fold_left ~init:false types ~f:(fun a (k, (atom, t)) ->
      let v =
        match atom, List.mem ~equal:String.equal keys k with
        | true, true -> BitmexType.to_type t
        | true, false -> BitmexType.to_type t ^ " option"
        | _ -> BitmexType.to_type t ^ " list"
      in
      Buffer.add_string
        buf
        (sprintf "    %s : %s ;\n" (String.uncapitalize k |> protect_key) v);
      match t with
      | Any | UserPreferences | AssetsConfigNetworkItem -> true
      | _ -> a)
  in
  Buffer.add_string
    buf
    (sprintf "  }%s\n\n" (if contains_any then "" else " [@@deriving sexp]"));
  Buffer.add_string buf {|  let of_yojson json = Yojson.Safe.Util.{
|};
  List.iter types ~f:(fun (k, (atom, t)) ->
    let k' = String.uncapitalize k |> protect_key in
    if List.mem ~equal:String.equal keys k
    then
      Buffer.add_string
        buf
        (sprintf "    %s = member \"%s\" json |> %s ;\n" k' k (extract_member t))
    else if not atom
    then
      Buffer.add_string
        buf
        (sprintf
           "    %s = member \"%s\" json |> to_list |> List.map %s ;\n"
           k'
           k
           (extract_member t))
    else
      Buffer.add_string
        buf
        (sprintf
           "    %s = (try Some (member \"%s\" json |> %s) with _ -> None) ;\n"
           k'
           k
           (extract_member t)));
  Buffer.add_string buf "  }\n\n";
  Buffer.add_string buf {|  let merge t t' = {
|};
  List.iter types ~f:(fun (k, (atom, _)) ->
    let k' = String.uncapitalize k |> protect_key in
    if List.mem ~equal:String.equal keys k
    then Buffer.add_string buf (sprintf "    %s = t'.%s ;\n" k' k')
    else if not atom
    then
      Buffer.add_string
        buf
        (sprintf
           "    %s = (match t.%s, t'.%s with (_, []) -> t.%s | _ -> t'.%s) ;\n"
           k'
           k'
           k'
           k'
           k')
    else
      Buffer.add_string
        buf
        (sprintf
           "    %s = (match t'.%s, t.%s with | Some a, _ -> Some a | _, Some b -> Some b \
            | _ -> None) ;\n"
           k'
           k'
           k'));
  Buffer.add_string buf "  }\n";
  Buffer.add_string buf "end";
  Buffer.contents buf
;;

let uuidm_module =
  {|
module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t = sexp_of_string (to_string t)
end|}
;;

let ptime_module =
  {|
module Ptime = struct
  include Ptime

  module Span = struct
    include Span
    let t_of_sexp sexp =
      let sexp_fl = float_of_sexp sexp in
      Option.get (of_float_s sexp_fl)

    let sexp_of_t t =
      sexp_of_float (to_float_s t)
  end

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t = sexp_of_string (to_rfc3339 t)
end|}
;;

let ptime_fun =
  {|
let ptime_of_string s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> failwith "ptime_of_string"
|}
;;

let () =
  let open Yojson.Safe in
  let schema = Stdio.In_channel.read_all (Sys.get_argv ()).(1) |> from_string in
  let modules = Util.keys schema in
  let nm = "AssetsConfigNetworkItem" in
  let modules = List.filter modules ~f:(fun x -> not (String.equal nm x)) @ [ nm ] in
  let modules = List.group modules ~break:String.equal in
  Stdio.Out_channel.with_file
    (Sys.get_argv ()).(2)
    ~append:false
    ~binary:false
    ~fail_if_exists:false
    ~f:(fun oc ->
      Stdio.Out_channel.fprintf oc "open Sexplib.Std\n\n";
      Stdio.Out_channel.fprintf oc "%s\n\n" uuidm_module;
      Stdio.Out_channel.fprintf oc "%s\n\n" ptime_module;
      Stdio.Out_channel.fprintf oc "%s\n\n" ptime_fun;
      List.iter modules ~f:(fun group ->
        List.iter (List.rev group) ~f:(fun name ->
          try
            Stdio.Out_channel.fprintf oc "%s" (gen_module name (Util.member name schema));
            Stdio.Out_channel.fprintf oc "\n\n"
          with
          | UnsupportedType -> ())))
;;
