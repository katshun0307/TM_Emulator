open Core
(* open Yojson *)
(* open Printexc *)

module Json_parser : sig
  type t = Yojson.Safe.json
  type getter =
    | FromDict of string
    | FromList of int

  exception Not_found of Yojson.Safe.json * getter
  exception Not_Compatible_Type of Yojson.Safe.json * getter list
  val string_of_not_found: exn -> string
  val string_of_not_compatible_type: exn -> string

  val init : string -> t
  val get_json: t -> getter list -> t
  val get_json_string: t -> getter list -> string
  val get_json_list: t -> getter list -> t list
  val get_json_int: t -> getter list -> int
  val get_json_float: t -> getter list -> float

end = struct

  type t = Yojson.Safe.json

  type getter = 
    | FromDict of string
    | FromList of int

  let string_of_getter = function
    | FromDict s -> Printf.sprintf "FromDict(%s)" s
    | FromList i -> Printf.sprintf "FromList(%d)" i

  let string_of_getter_list gl = 
    Printf.sprintf "[%s]" (String.concat ~sep:";" (List.map ~f:string_of_getter gl))

  exception Not_found of Yojson.Safe.json * getter
  exception Not_Compatible_Type of Yojson.Safe.json * getter list

  let string_of_not_found = function
    | Not_found(j, g) -> Printf.sprintf "Not_found(%s, %s)" (Yojson.Safe.to_string j) (string_of_getter g)
    | _ -> ""

  let string_of_not_compatible_type = function
    | Not_Compatible_Type(j, gl) -> Printf.sprintf "Not_compatible_type(%s, %s)" (Yojson.Safe.to_string j) (string_of_getter_list gl)
    | _ -> ""

  let init json_s = 
    Yojson.Safe.from_string json_s

  (* getter methods *)
  let rec get_nth = function
    | [], _ -> None
    | _, n when n < 0 -> None
    | x::_, 0 -> Some x
    | _::xs, n -> get_nth(xs, n-1)

  let rec get_json (json:Yojson.Safe.json) (g: getter list) = 
    match g with
    | hd:: tl -> (
        match json, hd with
        | (`Assoc kv_list, FromDict s) -> 
          (match List.Assoc.find kv_list ~equal:String.equal s with
           | Some json_val -> get_json json_val tl
           | None -> raise (Not_found (json, hd))
          )
        | (`List json_list, FromList i) -> 
          (match get_nth (json_list, i) with
           | Some json_val -> get_json json_val tl
           | None -> raise  (Not_found (json, hd))
          )
        | _ -> raise (Not_found (json, hd))
      )
    | [] -> json

  let get_json_string json g = 
    match get_json json g with
    | `String s -> s
    | _ as j -> raise (Not_Compatible_Type (j, g))

  let get_json_list json g = 
    match get_json json g with
    | `List l -> l
    | _ as j -> raise (Not_Compatible_Type(j, g))

  let get_json_int json g = 
    match get_json json g with
    | `Int i -> i
    | _ as j -> raise (Not_Compatible_Type(j, g))

  let get_json_float json g = 
    match get_json json g with
    | `Float i -> i
    | _ as j -> raise (Not_Compatible_Type(j, g))
end
