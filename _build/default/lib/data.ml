type transition = {
  actual_state: int;
  key_pressed: string;
  next_state: int;
}

type automaton = {
  alphabet: string list;
  starting_state : int;
  states: int list;
  final_states: (int * string) list;
  transitions: transition list;
  actual_state: int
}