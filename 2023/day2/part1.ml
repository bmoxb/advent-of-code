type colour_count = string * int

let valid_colour_count (colour, count) =
  let maximum =
    match colour with "red" -> 12 | "green" -> 13 | "blue" -> 14 | _ -> 0
  in
  count <= maximum

let valid_subset = List.for_all valid_colour_count

let parse_colour_count s =
  match String.split_on_char ' ' s with
  | [ num; colour ] -> (colour, int_of_string num)
  | _ -> raise Exit

let parse_subset s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.map parse_colour_count

let parse_subsets s =
  String.split_on_char ';' s |> List.map String.trim |> List.map parse_subset

let parse_game_id s = String.sub s 5 (String.length s - 5) |> int_of_string

let rec sum_valid_game_ids next_line =
  let determine_increase line =
    let colon_split = String.split_on_char ':' line in
    let id = List.nth colon_split 0 |> parse_game_id in
    let valid =
      List.nth colon_split 1 |> parse_subsets |> List.for_all valid_subset
    in
    if valid then id else 0
  in
  match next_line () with
  | Some line -> determine_increase line + sum_valid_game_ids next_line
  | None -> 0

let () =
  let chan = open_in "input.txt" in
  let next_line () = try Some (input_line chan) with End_of_file -> None in
  let answer = sum_valid_game_ids next_line in
  Printf.printf "Answer: %d\n" answer
