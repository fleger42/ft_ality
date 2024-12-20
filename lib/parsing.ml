let usage_msg =
  "usage: ft_ality [-h] grammarfile
  
  positional arguments:
  grammarfile            grammar description of the combos and keys
  
  optional arguments:
    -h, --help          show this help message and exit"

let speclist =
  [
    ("-h", Arg.Unit (fun () -> print_endline usage_msg; exit 0), " Show help message");
    ("--help", Arg.Unit (fun () -> print_endline usage_msg; exit 0), " Show help message");
  ]
let parse filename =
  let channel = open_in filename in
  try
    while true do
      let line = input_line channel in
      Printf.printf "%s\n" line
    done
  with End_of_file ->
    close_in channel
