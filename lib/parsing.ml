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

type automaton = {
  alphabet: string list;
  starting_state : int;
  states: int list;
  final_states: (int * string) list;
  transitions: Transition.transition list;
  actual_state: int
}
module TransitionMap = Map.Make(struct type t = int * string let compare = compare end)
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let usage_msg =
  "usage: ft_ality [-h] grammarfile
  
  grammarfile           grammar description of the combos and keys
  
  optional arguments:
    -h                show this help message and exit"

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
  Printf.printf "---------------------------\n"

let speclist =
  [
    ("-h", Arg.Unit (fun () -> print_endline usage_msg; exit 0), " Show help message");
  ]
let split_by_newline input =
String.split_on_char '\n' input

let anon_fun arg =
  match !args_value with
  | "" -> args_value := arg
  | _ -> ()


  let parse_key_state_mapping lines =
    let rec parse lines acc =
      match lines with
      | [] -> acc
      | line :: rest ->
          (* Ignore empty lines and split key:state pairs *)
          if line = "" then parse rest acc
          else if line = "-" then
            acc
          else
            let parts = String.split_on_char ':' line in
            match parts with
            | [key; state] -> parse rest ((key, state) :: acc)
            | _ -> parse rest acc
    in
    parse lines []

  let get_automaton_alphabet state_map = 
      List.map (fun (s1, _) -> s1) state_map

  let extract_after_dash data =
  let rec aux seen_dash = function
    | [] -> []  (* If list is empty, return empty list *)
    | "-" :: rest -> aux true rest  (* Mark when "-" is found *)
    | head :: rest when seen_dash -> head :: aux seen_dash rest  (* Collect elements after "-" *)
    | _ :: rest -> aux seen_dash rest  (* Skip until "-" is found *)
  in
  aux false data

  let parse_input input =
  (* Split the input by new lines first, as the input is multiline *)
  (* Split each line by ":" to separate the action from the description *)
  let split_lines = List.map (fun line ->
    match String.split_on_char ':' line with
    | [action; _] -> String.split_on_char ',' action
    | _ -> []
  ) input in
  split_lines

  let print_string_list_list lst =
  lst |> List.iter (fun sublist ->
    sublist |> List.iter (fun str -> print_string str; print_string " ");
    print_newline ()  (* Print a newline after each sublist *)
  )

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
            print_state_map state_map;
            Printf.printf "Looking up (%d, \"%s\") in state_map\n" current_state key;
            (* Check if the combination (current_state, key) already exists in the map *)
            let next_state, updated_map, updated_max =
              match TransitionMap.find_opt (current_state, key) state_map with
              | Some next ->
                  Printf.printf "Found\n"; (* Debugging line to confirm the state was found *)
                  next, state_map, max_state  (* Reuse the next_state and modify if necessary *)
              | None ->  (* Otherwise, create a new state: largest_state + 1 *)
                  Printf.printf "Not found, creating new state\n";
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
    
  let get_after_colon str =
    try
      let colon_index = String.index str ':' in
      String.sub str (colon_index + 1) (String.length str - colon_index - 1)
    with Not_found -> str  (* If no colon is found, return the original string *)
  
  let extract_parts_after_colon lines =
    List.map get_after_colon lines
  
    let create_pair_list keys values =
      try
        List.combine keys values
      with Invalid_argument _ ->
        failwith "Lists must have the same length"
        
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
  let create_automaton input_lines =
    
    let state_map = parse_key_state_mapping input_lines in
    let after_dash = extract_after_dash input_lines in
    let sequences = parse_input after_dash in
    let combo_map = extract_parts_after_colon after_dash in
    Printf.printf "after_dash: %s\n" (String.concat ", " combo_map);
    let transition_lists, final_states_key = transitions_from_sequences sequences in
    let final_states = create_pair_list final_states_key combo_map in
    let states = extract_unique_states transition_lists in
    {
      alphabet = get_automaton_alphabet state_map;
      starting_state = 0;
      actual_state = 0;
      states = states;
      final_states = final_states;
      transitions = transition_lists
    }
  
  (* Example input as a list of strings (simulating the input file) *)
  let input = [
    "left:Left";
    "right:Right";
    "up:Up";
    "down:Down";
    "q:HP";
    "s:LP";
    "-";
    "HP,LP,Down:Backflip Kick";
    "HP,HP,LP:Head Smash";
  ]
  

  let read_file filename =
    try
      (* Open the file for reading *)
      let channel = open_in filename in
  
      (* Function to read the file line by line and store each line in a list *)
      let rec read_lines acc =
        try
          (* Read a line from the file *)
          let line = input_line channel in
          (* Call read_lines recursively, adding the current line to the accumulator *)
          read_lines (line :: acc)
        with
        | End_of_file -> List.rev acc  (* When we reach the end of the file, reverse the list *)
        | e -> 
            close_in channel;  (* Make sure to close the file in case of error *)
            raise e  (* Reraise any other exception *)
      in
  
      (* Start reading lines *)
      let lines = read_lines [] in
      close_in channel;  (* Close the file after reading *)
      lines  (* Return the list of lines *)
  
    with
    | Sys_error msg -> 
        Printf.printf "Error opening file: %s\n" msg;
        exit 1
      
let parse () =
  Arg.parse speclist anon_fun usage_msg;
  let num_args = Array.length Sys.argv - 1 in
  if !args_value = "" || num_args > 1 then (
    print_endline usage_msg; exit 1
  );
  let grammar_content = read_file !args_value in
  let automaton = create_automaton grammar_content in
  print_automaton_data automaton