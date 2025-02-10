module Transition = struct
  type transition = {
    actual_state: int;
    key_pressed: string;
    next_state: int;
  }

  let print (transition: transition) =
    Printf.printf "(%d %s %d);\n"
      transition.actual_state transition.key_pressed transition.next_state
end

module TransitionMap = Map.Make(struct type t = int * string let compare = compare end)
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)
module StringMap = Map.Make(String)
open Tsdl

type automaton = {
  alphabet: string list;
  starting_state: int;
  states: int list;
  final_states: (int * string) list;
  transitions: Transition.transition list;
  actual_state: int;
}

let create_map pairs =
  List.fold_left (fun acc (key, value) -> StringMap.add key value acc) StringMap.empty pairs

let usage_msg =
  "usage: ft_ality [-h] grammarfile\n\n" ^
  "  grammarfile           grammar description of the combos and keys\n\n" ^
  "  optional arguments:\n" ^
  "    -h                show this help message and exit"

let args_value = ref ""

let print_automaton_alphabet automaton =
  Printf.printf "Alphabet: %s\n" (String.concat ", " automaton.alphabet)

let print_automaton_states automaton =
  Printf.printf "States: %s\n" (String.concat ", " (List.map string_of_int automaton.states))

let print_automaton_starting_state automaton =
  Printf.printf "Starting state: %d\n" automaton.starting_state

let print_automaton_actual_state automaton =
  Printf.printf "Actual state: %d\n" automaton.actual_state

let print_automaton_final_state automaton =
  Printf.printf "Final states: %s\n"
    (String.concat ", "
       (List.map (fun (s, label) -> Printf.sprintf "(%d, %s)" s label) automaton.final_states))

let print_automaton_transitions automaton =
  Printf.printf "Transitions:\n";
  List.iter Transition.print automaton.transitions

let print_automaton_data automaton =
  Printf.printf "Automaton data:\n";
  Printf.printf "---------------------------\n";
  print_automaton_alphabet automaton;
  print_automaton_states automaton;
  print_automaton_starting_state automaton;
  print_automaton_final_state automaton;
  print_automaton_actual_state automaton;
  print_automaton_transitions automaton;
  Printf.printf "---------------------------\n";
  flush stdout

let speclist =
  [ ("-h", Arg.Unit (fun () -> print_endline usage_msg; exit 0), " Show help message") ]

let anon_fun arg =
  match !args_value with
  | "" -> args_value := arg
  | _ -> ()

let parse_key_state_mapping lines =
  let rec parse lines acc =
    match lines with
    | [] -> acc
    | line :: rest ->
      if line = "" then parse rest acc
      else if line = "-" then acc
      else
        match String.split_on_char ':' line with
        | [key; state] -> parse rest ((key, state) :: acc)
        | _ -> parse rest acc
  in
  parse lines []

let get_automaton_alphabet state_map =
  List.map fst state_map

let extract_after_dash data =
  let rec aux seen_dash = function
    | [] -> []
    | "-" :: rest -> aux true rest
    | head :: rest when seen_dash -> head :: aux seen_dash rest
    | _ :: rest -> aux seen_dash rest
  in
  aux false data

let parse_input input =
  List.map (fun line ->
      match String.split_on_char ':' line with
      | [action; _] -> String.split_on_char ',' action
      | _ -> []
    ) input

let extract_parts_after_colon lines =
  List.map (fun str ->
      try String.sub str (String.index str ':' + 1) (String.length str - String.index str ':' - 1)
      with Not_found -> str
    ) lines

let create_pair_list keys values =
  try List.combine keys values
  with Invalid_argument _ -> failwith "Lists must have the same length"

let extract_unique_states transitions =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | {Transition.actual_state; Transition.key_pressed=_; Transition.next_state} :: rest ->
      let acc, seen =
        if IntSet.mem actual_state seen then acc, seen
        else actual_state :: acc, IntSet.add actual_state seen
      in
      let acc, seen =
        if IntSet.mem next_state seen then acc, seen
        else next_state :: acc, IntSet.add next_state seen
      in
      aux seen acc rest
  in
  aux IntSet.empty [] transitions

  let print_state_map state_map =
    Printf.printf "State map:\n";
    TransitionMap.iter (fun (state, key) next_state ->
      Printf.printf "{{State: %d, Key: \"%s\" -> Next State: %d}}\n" state key next_state
    ) state_map;
    Printf.printf "-------\n"

  let transitions_from_sequence sequence max_state state_map =
    let rec aux current_state max_state acc state_map = function
      | [] -> List.rev acc, state_map, max_state  (* Return transitions, map, and largest state *)
      | key :: rest ->
          (* Print the current state map to debug *)
          (*print_state_map state_map;*)
          (*Printf.printf "Looking up (%d, \"%s\") in state_map\n" current_state key;*)
          (* Check if the combination (current_state, key) already exists in the map *)
          let next_state, updated_map, updated_max =
            match TransitionMap.find_opt (current_state, key) state_map with
            | Some next ->
                next, state_map, max_state  (* Reuse the next_state and modify if necessary *)
            | None ->  (* Otherwise, create a new state: largest_state + 1 *)
                (*Printf.printf "Not found, creating new state\n";*)
                let next = max_state + 1 in
                next, TransitionMap.add (current_state, key) next state_map, next
          in
          let transition = {Transition.actual_state = current_state; key_pressed = key; next_state} in
          aux next_state updated_max (transition :: acc) updated_map rest
    in
    aux 0 max_state [] state_map sequence
  let transitions_from_sequences sequences =
    let _, transitions, max_states =
      List.fold_left (fun (acc_map, acc_transitions, acc_max_states) sequence ->
        let current_max_state = match acc_max_states with
          | [] -> 0  (* Use 0 as the initial state if the list is empty *)
          | hd :: _ -> hd  (* Otherwise, use the last maximum state *)
        in
        let transitions, updated_map, updated_max_state = transitions_from_sequence sequence current_max_state acc_map in
        updated_map, List.rev_append transitions acc_transitions, updated_max_state :: acc_max_states
      ) (TransitionMap.empty, [], []) sequences
    in
    List.rev transitions, List.rev max_states
let create_automaton input_lines =
  let state_map = parse_key_state_mapping input_lines in
  let after_dash = extract_after_dash input_lines in
  let sequences = parse_input after_dash in
  let combo_map = extract_parts_after_colon after_dash in
  let transition_lists, final_states_key = transitions_from_sequences sequences in
  let final_states = create_pair_list final_states_key combo_map in
  let states = extract_unique_states transition_lists in
  {
    alphabet = get_automaton_alphabet state_map;
    starting_state = 0;
    actual_state = 0;
    states = states;
    final_states = final_states;
    transitions = transition_lists;
  }

let read_file filename =
  try
    let channel = open_in filename in
    let rec read_lines acc =
      try read_lines (input_line channel :: acc)
      with End_of_file -> close_in channel; List.rev acc
    in
    read_lines []
  with Sys_error msg ->
    Printf.printf "Error opening file: %s\n" msg;
    exit 1

    let detect_keypress () =
      let event = Sdl.Event.create () in
      let rec loop () =
        if Sdl.poll_event (Some event) then
          let event_type = Sdl.Event.get event Sdl.Event.typ in
          if event_type = Sdl.Event.key_down then
            let keycode = Sdl.Scancode.enum (Sdl.Event.get event Sdl.Event.keyboard_scancode) in
            let modifiers = Sdl.Event.get event Sdl.Event.keyboard_keymod in
    
            (* Check if the Shift key is pressed using bitmask comparison *)
            let is_shift_pressed = (modifiers land Sdl.Kmod.shift <> 0) in
    
            (* Get the key string with shift handling *)
            let key_string = match keycode with
              | `Up -> "up"
              | `Down -> "down"
              | `Left -> "left"
              | `Right -> "right"
              | `Escape -> "esc"
              (* Handle alphabetic keys with Shift for uppercase *)
              | `A -> if is_shift_pressed then "A" else "a"
              | `B -> if is_shift_pressed then "B" else "b"
              | `C -> if is_shift_pressed then "C" else "c"
              | `D -> if is_shift_pressed then "D" else "d"
              | `E -> if is_shift_pressed then "E" else "e"
              | `F -> if is_shift_pressed then "F" else "f"
              | `G -> if is_shift_pressed then "G" else "g"
              | `H -> if is_shift_pressed then "H" else "h"
              | `I -> if is_shift_pressed then "I" else "i"
              | `J -> if is_shift_pressed then "J" else "j"
              | `K -> if is_shift_pressed then "K" else "k"
              | `L -> if is_shift_pressed then "L" else "l"
              | `M -> if is_shift_pressed then "M" else "m"
              | `N -> if is_shift_pressed then "N" else "n"
              | `O -> if is_shift_pressed then "O" else "o"
              | `P -> if is_shift_pressed then "P" else "p"
              | `Q -> if is_shift_pressed then "Q" else "q"
              | `R -> if is_shift_pressed then "R" else "r"
              | `S -> if is_shift_pressed then "S" else "s"
              | `T -> if is_shift_pressed then "T" else "t"
              | `U -> if is_shift_pressed then "U" else "u"
              | `V -> if is_shift_pressed then "V" else "v"
              | `W -> if is_shift_pressed then "W" else "w"
              | `X -> if is_shift_pressed then "X" else "x"
              | `Y -> if is_shift_pressed then "Y" else "y"
              | `Z -> if is_shift_pressed then "Z" else "z"
              | `Lshift -> ""
              | _ -> ""  (* For non-alphabetic keys *)
            in
            key_string  (* Return the key string *)
    
          else loop ()  (* Continue looping if not a keydown event *)
        else loop ()  (* If no event, continue polling *)
      in
      loop ()
    

let find_transition key_action transitions actual_state =
  List.find_opt (fun t -> t.Transition.key_pressed = key_action && t.actual_state = actual_state) transitions
  
let is_final_state state final_states =
  List.assoc_opt state final_states
  
let check_multiple_combo key_action transitions next_state =
    let matching_transitions =
      List.filter (fun t -> t.Transition.key_pressed = key_action && t.next_state = next_state) transitions
    in
    List.length matching_transitions > 1
  
  let rec execution automaton input_map =
    let key = detect_keypress () in
    Printf.printf "Pressed key %s\n" key; 
    flush stdout;
    if key = "" then
      execution automaton input_map
    else if key != "esc" then
      (* If the key isn't ESC, continue processing the input *)
      begin
        let key_action = match StringMap.find_opt key input_map with
          | Some value -> value
          | None -> "" 
        in
  
        let next_state =
          match find_transition key_action automaton.transitions automaton.actual_state with
          | Some t -> 
              let check_final_state =
                match is_final_state t.next_state automaton.final_states with
                | Some s -> 
                  Printf.printf "[%s] Combo ! %d\n" s t.next_state; 
                  flush stdout;
                    s
                | None -> ""
                in
                if (check_final_state <> "" && (check_multiple_combo key_action automaton.transitions t.next_state) == false) then
                begin
                  Printf.printf "Reset..\n";
                  flush stdout;
                  automaton.starting_state
                end
                else
                  t.next_state
          | None ->
            Printf.printf "Reset..\n"; 
            flush stdout; 
            automaton.starting_state
        in
        let updated_automaton = {
          alphabet = automaton.alphabet;
          starting_state = automaton.starting_state;
          actual_state = next_state;
          states = automaton.states;
          final_states = automaton.final_states;
          transitions = automaton.transitions;
        } in
        execution updated_automaton input_map
      end
    else 
      begin
        Printf.printf "Exiting program...\n"; 
        flush stdout;
        Sdl.quit ();
      end
    
  
let parse () =
  Arg.parse speclist anon_fun usage_msg;
  if !args_value = "" || Array.length Sys.argv > 2 then (print_endline usage_msg; exit 1);
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> failwith ("SDL Initialization Error: " ^ e)
  | Ok () ->
      match Sdl.create_window "Invisible Window" ~w:1 ~h:1 Sdl.Window.opengl with
      | Error (`Msg e) -> failwith ("Window creation failed: " ^ e)
      | Ok _window ->
          let input_lines = read_file !args_value in
          let automaton = create_automaton input_lines in
          List.iter (fun str -> Printf.printf "%s\n" str) input_lines;
          Printf.printf "Press an arrow key (ESC to exit)...\n";
          flush stdout;
          execution automaton (create_map (parse_key_state_mapping input_lines))
