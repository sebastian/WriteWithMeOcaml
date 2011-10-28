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

(* Add a string to the trie *)
let add_text trie text =
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
  in add_words(word_char_list, trie)

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
    let start_values = if trie.word_count > 0 then
      (* We have a potential new candidate *)
      (current_word, trie.word_count)
    else (curr_next, curr_next_count) in
    List.fold_left (fun (w, c) child ->
      best_child child;
      let (ww, cc) = child_finder child current_word w c in
      if c > cc then (w, c) else (ww, cc)) start_values children
  and best_child element =
    let (best_word, _count) = List.fold_left (fun (b, c) new_branch ->
      let (w, cc) = child_finder new_branch [] b c in
      if c > cc then (b, c) else (w, cc)) ([], 0) element.new_word in
    element.best_next_word <- string_of_chars (List.rev best_word);
    ()
  in best_child trie

(* Creates an empty trie tree *)
let words = entry_for_character '-'