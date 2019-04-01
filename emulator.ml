open Core
open Json_parser.Json_parser

exception Undefined_transition
exception End_of_tape
exception Corrupt_tape

let show_left = ref 10
let show_total = ref 20

type symbols = 
  | Blank
  | Alphabet of string

let string_of_symbols s = 
  match s with
  | Blank -> "_B_"
  | Alphabet a -> a

type states = 
  | InitialState
  | State of int
  | EndState

let string_of_states s = 
  match s with
  | InitialState -> "QI"
  | State i -> "Q" ^ (string_of_int i)
  | EndState -> "QE"

type moves = 
  | Left
  | Middle
  | Right

let string_of_moves m = 
  match m with
  | Left -> "left"
  | Middle -> "middle"
  | Right -> "right"

type transition = {
  target : states;
  write : symbols;
  move : moves;
}

let string_of_transition {target; write; move} =
  sprintf "{target: %s; write: %s; move: %s}" (string_of_states target) (string_of_symbols write) (string_of_moves move)  

type transition_rule = {
  from : states;
  read : symbols;
  trans : transition
}

let string_of_transition_rule r = 
  sprintf "{from: %s; read: %s;
  trans: %s" 
    (string_of_states r.from) (string_of_symbols r.read)
    (string_of_transition r.trans)

let transitions = ref ([]: transition_rule list)

type tape =
  | Head
  | Column of {mutable left : tape; mutable content : symbols; mutable right : tape}
  | Tail

let get_left_tape_contents t = 
  let rec loop t accum = 
    match t with
    | Column {left=Head; content; _} -> content :: accum
    | Column cc -> loop cc.left (cc.content :: accum)
    | Head -> accum
    | _ -> raise End_of_tape
  in List.rev (List.tl_exn (List.rev (loop t [])))

let get_right_tape_contents t = 
  let rec loop t accum = 
    match t with
    | Column {content;right=Tail;_} -> content :: accum
    | Column cc -> loop cc.right (cc.content :: accum)
    | _ -> accum
  in (List.rev (loop t []))

let string_of_tape_contents t =
  let lefts = get_left_tape_contents t in
  let right_m = get_right_tape_contents t in
  let rights = List.tl_exn right_m in
  let middle = List.hd_exn right_m in
  "[" ^ String.concat ~sep:"|" (List.map ~f:string_of_symbols lefts)
  ^ "|*" ^ string_of_symbols middle ^ "*"
  ^ "|" ^ String.concat ~sep:"|" (List.map ~f:string_of_symbols rights) ^ "]"

type head = {
  mutable arrow : tape;
  mutable current_state : states
}

let string_of_head h step = 
  sprintf 
    "--------\nstep : %s\ncurrent_state : %s\ntape_content : %s\n"
    (string_of_int step) (string_of_states h.current_state) (string_of_tape_contents h.arrow) 

type machine = {
  head: head;
}

let get_tape_content (t: tape) = 
  match t with
  | Column {content=c;_} -> c
  | Head -> raise End_of_tape
  | Tail -> raise End_of_tape

let write_tape_content (t: tape) (s: symbols) = 
  match t with
  | Column cc -> cc.content <- s
  | Head -> raise End_of_tape
  | Tail -> raise End_of_tape

let get_right_tape (t: tape) = 
  match t with
  | Head -> Column{left=Head; content=Blank; right=Tail}
  | Column cc -> 
    (match cc.right with
     | Column _ -> cc.right
     | Tail -> cc.right <- Column{left=t; content=Blank; right=Tail};
       cc.right
     | Head -> raise Corrupt_tape)
  | Tail -> raise End_of_tape

let get_left_tape (t: tape) = 
  match t with
  | Head -> raise End_of_tape
  | Column cc -> 
    (match cc.left with
     | Column _ -> cc.left
     | Head -> cc.left <- Column{left=Head; content=Blank; right=t};
       cc.left
     | Tail -> raise Corrupt_tape)
  | Tail -> raise End_of_tape

let get_next_trans from read = 
  match List.find !transitions ~f:(fun {from=f; read=r; _} -> f = from && read = r) with
  | Some trans -> trans.trans
  | None -> raise Undefined_transition

let init_tape_from_list l =
  let start_tape = Column{left=Head;content=Blank;right=Tail} in
  let rec loop l t = 
    match l with
    | hd:: tl -> let new_t = get_left_tape t in
      write_tape_content new_t hd;
      loop tl new_t
    | [] -> t
  in loop (List.rev l) start_tape

let run_step (m: machine) = 
  let next_trans = get_next_trans (m.head.current_state) (get_tape_content m.head.arrow) in
  print_string ("next_trans: " ^ string_of_transition next_trans ^ "\n");
  (* write to tape *)
  write_tape_content m.head.arrow next_trans.write;
  (* change state *)
  m.head.current_state <- next_trans.target;
  (* move head *)
  (match next_trans.move with
   | Left -> m.head.arrow <- get_left_tape m.head.arrow
   | Right -> m.head.arrow <- get_right_tape m.head.arrow
   | Middle -> ());
  next_trans.target <> EndState

let run_all (m:machine) = 
  let cont = ref true in
  let step = ref 0 in
  while !cont do
    print_string (string_of_head m.head !step);
    let has_next = run_step m in
    if not has_next then cont := false else ();
    step := !step + 1;
  done;
  print_string (string_of_head m.head !step ^ 
                (sprintf "\n === finished with total steps of %i ===\n" !step) )

let read_init_tape j = 
  let json_list = get_json_list j [] in
  let str_list = List.map json_list 
      ~f:(fun j -> 
          let raw_str = get_json_string j [] in
          if raw_str = "B" then Blank else Alphabet raw_str ) 
  in init_tape_from_list str_list

let read_trans_rule j = 
  let from_ = get_json_int j [FromDict "from"] in
  let read_ = get_json_string j [FromDict "read"] in
  let trans_j = get_json j [FromDict "trans"] in
  let target_ = get_json_int trans_j [FromDict "target"] in
  print_string (string_of_int target_);
  let write_ = get_json_string trans_j [FromDict "write"] in
  let move_ = get_json_string trans_j [FromDict "move"] in
  {
    from = if from_ = -2 then InitialState else if from_ = -1 then EndState else State from_;
    read = if read_ = "B" then Blank else Alphabet read_;
    trans = {
      target = if target_ = -2 then InitialState else if target_ = -1 then EndState else State target_;
      write = if write_ = "B" then Blank else Alphabet write_;
      move = if move_ = "right" then Right else if move_ = "left" then Left else raise (Invalid_argument "error in json file")
    }
  }

let read_from_file filename = 
  let j_s = init (In_channel.read_all filename) in
  let init_tape = read_init_tape (get_json j_s [FromDict "tape"]) in
  let trans_rules = List.map (get_json_list j_s [FromDict "rules"]) 
      ~f:(fun j -> read_trans_rule j) in
  transitions := trans_rules;
  print_string (String.concat ~sep:"\n" (List.map trans_rules ~f:string_of_transition_rule) ^ "\n");
  {head={arrow=init_tape; current_state=InitialState}}

let main f = 
  let m = read_from_file f in
  run_all m

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command = 
  Command.basic
    ~summary:"Turing Machine Emulator"
    ~readme:(fun () -> "This program emulates a turing machine with transition functions as json input.")
    (Command.Param.map filename_param ~f:(fun filename -> (fun () -> main filename)))

let _ = 
  Command.run ~version:"1.0" command;
  (* let init_tape = init_tape_from_list [Alphabet "1";Alphabet "1";Blank] in
     let m = {head={arrow=init_tape; current_state=InitialState}} in
     run_all m; *)
