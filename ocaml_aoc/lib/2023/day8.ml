open Core

let rec map_network_path network_list ~map =
  match network_list with
  | [] -> map
  | network_node :: rest ->
    let new_map =
      Scanf.sscanf network_node "%s = (%s@, %s@)" (fun node left right ->
        Map.set map ~key:node ~data:(left, right))
    in
    map_network_path rest ~map:new_map
;;

let parse_data input =
  let empty_map = Map.empty (module String) in
  let separated_input = input |> Str.split (Str.regexp "\n\n") in
  let moves = List.hd_exn separated_input |> String.to_list in
  let network_map =
    List.nth_exn separated_input (List.length separated_input - 1)
    |> String.split ~on:'\n'
    |> map_network_path ~map:empty_map
  in
  moves, network_map
;;

let rec calculate_ways moves network_map current_node =
  match moves with
  | [] -> failwith "No path found"
  | current_move :: rest ->
    (match current_node with
     | "ZZZ" -> 0
     | key ->
       let left, right = Map.find_exn network_map key in
       (match current_move with
        | 'L' -> 1 + calculate_ways (rest @ [ current_move ]) network_map left
        | 'R' -> 1 + calculate_ways (rest @ [ current_move ]) network_map right
        | _ -> failwith "Invalid move"))
;;

let part_one input =
  let move_list, network_map = parse_data input in
  calculate_ways move_list network_map "AAA"
;;

let example = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"

module Exec = struct
  let run () =
    print_endline "Day 8:";
    let input = Helper.FileReader.read_file "../inputs/2023/day8-puzzle.txt" in
    printf "Part 1: %d\n" (part_one input);
    print_endline "-------------------------"
  ;;
end
