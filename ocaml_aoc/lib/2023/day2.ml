open Core

type bag_of_gems =
  { red : int
  ; green : int
  ; blue : int
  }

let match_game_id_digits = function
  | '1' -> '1'
  | '2' -> '2'
  | '3' -> '3'
  | '4' -> '4'
  | '5' -> '5'
  | '6' -> '6'
  | '7' -> '7'
  | '8' -> '8'
  | '9' -> '9'
  | '0' -> '0'
  | _ -> ' '
;;

let get_game_id input =
  let raw_game_id =
    String.sub input ~pos:4 ~len:(String.length input - 4)
    |> String.map ~f:match_game_id_digits
    |> String.strip
  in
  Int.of_string raw_game_id
;;

let extract_gem_count gem = String.sub gem ~pos:0 ~len:2 |> String.strip |> Int.of_string

let generate_bag_of_gem acc gem =
  match String.strip gem with
  | x when String.is_suffix ~suffix:"red" x -> { acc with red = extract_gem_count x }
  | x when String.is_suffix ~suffix:"green" x -> { acc with green = extract_gem_count x }
  | x when String.is_suffix ~suffix:"blue" x -> { acc with blue = extract_gem_count x }
  | _ -> acc
;;

let get_one_bag input =
  String.strip input
  |> String.split ~on:','
  |> List.fold_left ~init:{ red = 0; green = 0; blue = 0 } ~f:generate_bag_of_gem
;;

let get_highest_possible_bag_of_gem input =
  let empty_bag = { red = 0; green = 0; blue = 0 } in
  let game_records = String.split ~on:';' input in
  let rec find_highest_gem_in_bag records max_bag_of_gem =
    match records with
    | [] -> max_bag_of_gem
    | r :: rs ->
      let bag = get_one_bag r in
      find_highest_gem_in_bag
        rs
        { red = (if bag.red > max_bag_of_gem.red then bag.red else max_bag_of_gem.red)
        ; green =
            (if bag.green > max_bag_of_gem.green then bag.green else max_bag_of_gem.green)
        ; blue =
            (if bag.blue > max_bag_of_gem.blue then bag.blue else max_bag_of_gem.blue)
        }
  in
  find_highest_gem_in_bag game_records empty_bag
;;

let get_game_id_when_sum_of_gems_are_valid input =
  let input_list = String.split ~on:':' input in
  let game_id = get_game_id (List.hd_exn input_list) in
  let bag_of_gem = input_list |> List.last_exn |> get_highest_possible_bag_of_gem in
  if bag_of_gem.red <= 12 && bag_of_gem.green <= 13 && bag_of_gem.blue <= 14
  then game_id
  else 0
;;

let rec part_one_solution input_list =
  match input_list with
  | [] -> 0
  | x :: xs -> get_game_id_when_sum_of_gems_are_valid x + part_one_solution xs
;;

module Exec = struct
  let run () =
    print_endline "Day 2:";
    let inputs = Helper.FileReader.read_file "../inputs/2023/day2-puzzle.txt" in
    let input_list = String.split ~on:'\n' inputs in
    printf "Part 1: %d\n" (part_one_solution input_list);
    print_endline "-------------------------"
  ;;
end
