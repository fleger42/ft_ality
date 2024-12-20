type transition = {
  actual_state: int;
  key_pressed: string;
  next_state: int;
}

type automaton = {
  alphabet: string list;
  states: int list;
  starting_state : int;
  final_states: int list;
  transitions: transition list;
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
          else
            let parts = String.split_on_char ':' line in
            match parts with
            | [key; state] -> parse rest ((key, state) :: acc)
            | _ -> parse rest acc
    in
    parse lines []
  
  (* Parse the transition part of the input (sequences and resulting states) *)
  let parse_transitions lines state_map =
    let rec parse_transitions lines acc =
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
            | _ -> parse_transitions rest acc
    in
    parse_transitions lines []
  
  (* Function to create the automaton *)
  let create_automaton input_lines =
    
    let state_map = parse_key_state_mapping input_lines in

    let transitions = parse_transitions input_lines state_map in
    let actual_state = 1 in
    (* For simplicity, assuming state "0" is the starting state, and no final states are defined *)
    {
      alphabet = List.map fst state_map;
      states = [0; 1]; (* Hardcoded states for now, should be inferred *)
      starting_state = 0;
      final_states = [1]; (* Hardcoded final state *)
      transitions = transitions;
      actual_state = actual_state
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
        []  (* Return an empty list in case of error *)
            
let parse () =
  Arg.parse speclist anon_fun usage_msg;
  let num_args = Array.length Sys.argv - 1 in
  if !args_value = "" || num_args > 1 then (
    exit 1
  );
  let grammar_content = read_file !args_value in
   let automaton = create_automaton grammar_content in
  Printf.printf "Alphabet: %s\n" (String.concat ", " automaton.alphabet);
    Printf.printf "States: %s\n" (String.concat ", " (List.map string_of_int automaton.states));
