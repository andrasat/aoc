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

let match_and_sum_gems acc gem =
  match gem with
  | x when String.is_suffix ~suffix:"red" x ->
    { acc with
      red = acc.red + (String.sub x ~pos:0 ~len:2 |> String.strip |> Int.of_string)
    }
  | x when String.is_suffix ~suffix:"green" x ->
    { acc with
      green = acc.green + (String.sub x ~pos:0 ~len:2 |> String.strip |> Int.of_string)
    }
  | x when String.is_suffix ~suffix:"blue" x ->
    { acc with
      blue = acc.blue + (String.sub x ~pos:0 ~len:2 |> String.strip |> Int.of_string)
    }
  | _ -> acc
;;

let get_one_bag input =
  String.strip input
  |> String.split ~on:','
  |> List.fold_left ~init:{ red = 0; green = 0; blue = 0 } ~f:match_and_sum_gems
;;

let check_if_bag_of_gems_possible input =
  let empty_bag = { red = 0; green = 0; blue = 0 } in
  let game_records = String.split ~on:';' input in
  let rec get_sum_of_bags records sum_of_bags =
    match records with
    | [] -> sum_of_bags
    | r :: rs ->
      let bag = get_one_bag r in
      get_sum_of_bags
        rs
        { red = sum_of_bags.red + bag.red
        ; green = sum_of_bags.green + bag.green
        ; blue = sum_of_bags.blue + bag.blue
        }
  in
  let final_bag = get_sum_of_bags game_records empty_bag in
  printf "final_bag: %d, %d, %d\n" final_bag.red final_bag.green final_bag.blue;
  if final_bag.red < 12 && final_bag.green < 13 && final_bag.blue < 14
  then true
  else false
;;

let get_game_id_when_sum_of_gems_are_valid input =
  let input_list = String.split ~on:':' input in
  let game_id = get_game_id (List.hd_exn input_list) in
  let is_possible = input_list |> List.last_exn |> check_if_bag_of_gems_possible in
  printf "game_id: %d, is_possible: %b\n" game_id is_possible;
  if is_possible then game_id else 0
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
