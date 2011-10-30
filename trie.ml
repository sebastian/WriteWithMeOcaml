type entry = { character : char;
               mutable best_next_word: string; 
               mutable word_count: int; 
               mutable child_count: int; 
               mutable next: entry list;
               mutable new_word: entry list }

let entry_for_character = function c ->
  { character = c;
    best_next_word = "";
    word_count = 0;
    child_count = 0;
    next = [];
    new_word = []; }

(* splits text into character list *)
let text_to_char_list text =
  let rec split = function
    | (w, cs, -1) -> cs
    | (w, cs,  i) -> split (w, (w.[i] :: cs), (i-1))
  in split (text, [], (String.length text - 1));;

(* prepares text for being added to the trie *)
(* more specifically, the string is:
    * lowercased
    * stripped for illegal characters
    * given __ as word deliminator           *)
let prep_text text =
  let chars = text_to_char_list (String.lowercase text) in
  let clean = List.filter (fun c ->
    match c with
    | '!' | '?' | '@' | '$' | '%' | '^' | '&' | '*' | '(' | ')' | '_' | '-' -> false
    | _ -> true) chars in
  let string = List.map (fun c ->
    match c with
    | ' ' | '\n' | '\t' -> '_'
    | n -> n) clean in
  let rec splitter = function
    | (acc, word, []) -> List.rev(List.rev(word) :: acc)
    | (acc, word, '_'::cs) -> splitter (List.rev(word) :: acc, [], cs)
    | (acc, word, c::cs) -> splitter (acc, c :: word, cs) in
  splitter ([], [], string)

let _efc c entry entry_setter =
  match (List.partition (fun e -> e.character == c) entry) with
      ([], rest) -> 
        let e = entry_for_character c in
        entry_setter (e :: rest);
        e
    | (item, rest) -> List.hd(item)

(* returns an entry for a letter, creating it if needed *)
let entry_for_character_in_new_word c ce = 
  let setter = (fun e -> ce.new_word <- e; ce.child_count <- ce.child_count + 1) in
  _efc c ce.new_word setter 
let entry_for_character c ce = _efc c ce.next (fun e -> ce.next <- e)

(* chop of the first word of a string *)
let less_first_word text = 
  try
    let first_space = (String.index_from text 0 ' ') + 1 in
    String.sub text first_space ((String.length text) - first_space)
  with Not_found -> ""

(* Add a string to the trie *)
let rec add_text = function
  | (trie, "") -> ()
  | (trie, text) ->
    let word_char_list = prep_text text in
    let increment_word_count w = w.word_count <- w.word_count + 1 in
    let rec traverse = function
      (* we have reached the end of the last word *)
      | ([], [], trie, _new_word) ->
          (* we are at the end of a word. Count it *)
          increment_word_count trie;
      (* we have reached the end of a word *)
      | ([], word::words, trie, _new_word) ->
          (* update the count for the current word *)
          increment_word_count trie;
          traverse (word, words, trie, true) 
      (* we have a word, plus additional words *)
      | (c::cs, words, trie, false) ->
          let e = entry_for_character c trie in traverse (cs, words, e, false)
      (* we have a word, and the current character is the first in the word *)
      | (c::cs, words, trie, true) ->
          let e = entry_for_character_in_new_word c trie in traverse (cs, words, e, false) 
    in let add_words = function
      | ([], trie) -> traverse ([], [], trie, true)
      | ((w::ws), trie) -> traverse (w, ws, trie, true) 
    in 
      add_words(word_char_list, trie);
      add_text(trie, (less_first_word text))

(* converts a list of chars to a string *)
let string_of_chars chars =
  let string = String.create (List.length chars) in
  let _ = List.fold_left (fun i c -> string.[i] <- c; i + 1) 0 chars in
  string

(* walks through the trie, calculating the most likely*)
(* next word for all words.                           *)
let normalize trie =
  let rec child_finder trie word_acc curr_next curr_next_count =
    let current_word = trie.character :: word_acc in
    let children = trie.next in
    let start_values = if (trie.word_count > 0) && (trie.word_count > curr_next_count) then
      (* We have a potential new candidate *)
      (current_word, trie.word_count)
    else (curr_next, curr_next_count) in
    (* look through all the subsequent words *)
    best_child trie;
    (* find the current best word *)
    List.fold_left (fun (w, c) child ->
      let (ww, cc) = child_finder child current_word w c in
      if c > cc then (w, c) else (ww, cc)) start_values children
  and best_child element =
    let (best_word, _count) = List.fold_left (fun (b, c) new_branch ->
      let (w, cc) = child_finder new_branch [] b c in
      if c > cc then (b, c) else (w, cc)) ([], 0) element.new_word in
    element.best_next_word <- string_of_chars (List.rev best_word);
    ()
  in best_child trie

(* returns the next word after a given string *)
let rec next_word trie word_list =
  let rec find_next = function
    | (tries, []) -> ""
    | (tries, word::words) ->
        try let next_trie = List.find (fun e -> e.character == List.hd word) tries in
          follow_word (next_trie, (List.tl word)::words)
        with Not_found -> next_word trie (List.tl word_list)
  and follow_word = function
    (* we are at the end of the sentence *)
    | (trie, [[]]) -> trie.best_next_word
    (* we have reached the end of a word *)
    | (trie, []::words) -> find_next(trie.new_word, words)
    (* we progress through the trie *)
    | (trie, words) -> find_next(trie.next, words)
  in follow_word (trie, []::word_list)

(* finds the next n words that follow a string of words *)
let next_words = function
  | (trie, "", _number) -> trie.best_next_word
  | (trie, text, number) -> 
      let rec finder = function
        | (_trie, _text, new_text, 0) -> new_text
        | (trie, text, new_text, number) ->
            let word_char_list = prep_text text in
            match (next_word trie (word_char_list)) with
              | "" -> new_text
              | "." -> finder(trie, text ^ ".", new_text ^ ".", number - 1)
              | "," -> finder(trie, text ^ ".", new_text ^ ",", number - 1)
              | word -> finder(trie, text ^ " " ^ word, new_text ^ " " ^ word, number - 1)
      in finder(trie, text, "", number)

(* Creates an empty trie tree *)
let words = entry_for_character '-'

(* let t = { character = '-'; best_next_word = ""; word_count = 0; child_count = 0; next = []; new_word = []; };; *)