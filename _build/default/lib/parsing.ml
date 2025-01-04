module Transition = struct
  type transition = {
    actual_state: int;
    key_pressed: string;
    next_state: int;
  }
  let print (transition: transition) =
    Printf.printf "  actual_state: %d, key_pressed: %s, next_state: %d\n"
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

let usage_msg =
  "usage: ft_ality [-h] grammarfile
  
  grammarfile           grammar description of the combos and keys
  
  optional arguments:
    -h                show this help message and exit"

let args_value = ref ""

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

  (* Parse the transition part of the input (sequences and resulting states) *)
 (*let rec parse_transitions lines =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
        (* Ignore empty lines *)
        if line = "" then parse_transitions rest acc
        else
          (* Parse transition line in format: key_sequence:resulting_state *)
          let parts = String.split_on_char ':' line in
          List.iter print_endline parts;
          match parts with
          | [key_seq; result_state] ->
            (* Split the sequence by commas *)
            let keys = String.split_on_char ':' key_seq in
            let next_state = int_of_string result_state in
              (* Find the actual state for each key in the sequence *)
              let actual_state = 
                try
                  List.assoc (List.hd keys) state_map (* Assume the first key corresponds to the initial state *)
                with Not_found -> "unknown" (* Error handling if key is not found *)
              in
              parse_transitions rest ({actual_state = int_of_string actual_state; key_pressed = key_seq; next_state} :: acc)
          | _ -> parse_transitions rest acc*)
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
  let create_automaton input_lines =
    
    let state_map = parse_key_state_mapping input_lines in
    let after_dash = extract_after_dash input_lines in
    let combo_map = parse_input after_dash in
    print_string_list_list combo_map;
    {
      alphabet = get_automaton_alphabet state_map;
      starting_state = 0;
      actual_state = 0;
      (* need to finish*)
      states = [0];
      final_states = [(0, "final_states")];
      transitions = [
        { Transition.actual_state = 0; Transition.key_pressed = "a"; Transition.next_state = 1 };
        { Transition.actual_state = 1; Transition.key_pressed = "b"; Transition.next_state = 2 };
        { Transition.actual_state = 2; Transition.key_pressed = "c"; Transition.next_state = 0 };
      ]
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
      
let parse () =
  Arg.parse speclist anon_fun usage_msg;
  let num_args = Array.length Sys.argv - 1 in
  if !args_value = "" || num_args > 1 then (
    print_endline usage_msg; exit 1
  );
  let grammar_content = read_file !args_value in
  let automaton = create_automaton grammar_content in
  print_automaton_data automaton