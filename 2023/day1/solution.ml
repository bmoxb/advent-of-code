let get_number line =
  let rec get_digit line update_index index =
    match line.[index] with
    | '0' .. '9' as c -> int_of_char c - int_of_char '0'
    | _ -> get_digit line update_index (update_index index)
  in
  let left = get_digit line (fun x -> x + 1) 0 in
  let right = get_digit line (fun x -> x - 1) (String.length line - 1) in
  (left * 10) + right

let rec sum_of_numbers_in_lines next_line =
  match next_line () with
  | Some line -> get_number line + sum_of_numbers_in_lines next_line
  | None -> 0

let () =
  let chan = open_in "input.txt" in
  let next_line () = try Some (input_line chan) with End_of_file -> None in
  let result = sum_of_numbers_in_lines next_line in
  Printf.printf "Answer: %d\n" result
