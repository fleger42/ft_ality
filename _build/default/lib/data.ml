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
}