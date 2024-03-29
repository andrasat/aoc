(* get values of min max in pairs *)
let get_min_max (range_list : string list) =
  let first_range =
    List.nth range_list 0 |> String.split_on_char '-' |> List.map int_of_string
  in
  let second_range =
    List.nth range_list 1 |> String.split_on_char '-' |> List.map int_of_string
  in
  ( List.nth first_range 0
  , List.nth first_range 1
  , List.nth second_range 0
  , List.nth second_range 1 )
;;

(* Q1 answer: check if pair contain each other *)
let get_point_if_contain (pair : string) =
  let pair_list = String.split_on_char ',' pair in
  let first_min, first_max, second_min, second_max = get_min_max pair_list in
  if (first_min <= second_min && first_max >= second_max)
     || (second_min <= first_min && second_max >= first_max)
  then 1
  else 0
;;

(* Q2 answer: check if pair overlap each other *)
let get_point_if_overlap (pair : string) =
  let pair_list = String.split_on_char ',' pair in
  let first_min, first_max, second_min, second_max = get_min_max pair_list in
  if first_max >= second_min && second_max >= first_min then 1 else 0
;;

(* check each data list *)
let rec sum_of_assignment_pairs pair_lists (f : string -> int) =
  match pair_lists with
  | [] -> 0
  | pair :: pair_lists -> f pair + sum_of_assignment_pairs pair_lists f
;;

module Exec = struct
  let run () =
    print_endline "Day 4: ";
    let inputs = Helper.FileReader.read_file "../inputs/2022/day4-puzzle.txt" in
    let input_list = String.split_on_char '\n' inputs in
    Printf.printf "Q1: %d\n" (sum_of_assignment_pairs input_list get_point_if_contain);
    Printf.printf "Q2: %d\n" (sum_of_assignment_pairs input_list get_point_if_overlap);
    print_endline "-------------------------"
  ;;
end
