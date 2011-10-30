type entry = {
  character : char;
  mutable best_next_word : string;
  mutable word_count : int;
  mutable child_count : int;
  mutable next : entry list;
  mutable new_word : entry list;
}
val text_to_char_list : string -> char list
val prep_text : string -> char list list
val _efc : char -> entry list -> (entry list -> 'a) -> entry
val entry_for_character_in_new_word : char -> entry -> entry
val entry_for_character : char -> entry -> entry
val less_first_word : string -> string
val add_text : entry * string -> unit
val string_of_chars : char list -> string
val normalize : entry -> unit
val next_word : entry -> char list list -> string
val next_words : entry * string * int -> string
val words : entry -> entry
