let word_num_pairs =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let starts_with_digit_word s =
  let predicate (word, _) = String.starts_with ~prefix:word s in
  List.find_opt predicate word_num_pairs |> Option.map (fun (_, num) -> num)

let is_digit = function '0' .. '9' -> true | _ -> false

let get_number line =
  let rec search line update_index index =
    let c = line.[index] in
    if is_digit c then int_of_char c - int_of_char '0'
    else
      let substr = String.sub line index (String.length line - index) in
      match starts_with_digit_word substr with
      | Some digit -> digit
      | None -> search line update_index (update_index index)
  in
  let left = search line (fun x -> x + 1) 0 in
  let right = search line (fun x -> x - 1) (String.length line - 1) in
  (left * 10) + right

let rec sum_numbers next_line =
  match next_line () with
  | Some line -> get_number line + sum_numbers next_line
  | None -> 0

let () =
  let chan = open_in "input.txt" in
  let next_line () = try Some (input_line chan) with End_of_file -> None in
  let result = sum_numbers next_line in
  Printf.printf "Answer: %d\n" result
