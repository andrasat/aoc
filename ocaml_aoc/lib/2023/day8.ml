open Core

module Nodes = struct
  module Map = Map.Make (String)

  type t = (string * string) Map.t
end

let scan_node input =
  Scanf.sscanf input "%s = (%s@, %s@)" (fun node left right -> node, (left, right))
;;

let map_network_path network_list =
  let map = network_list |> List.map ~f:scan_node |> Nodes.Map.of_alist in
  match map with
  | `Duplicate_key _ -> failwith "Duplicate key"
  | `Ok map -> map
;;

let parse_data input =
  let separated_input = input |> Str.split (Str.regexp "\n\n") in
  let moves = List.hd_exn separated_input |> String.to_list in
  let network_map =
    List.nth_exn separated_input (List.length separated_input - 1)
    |> String.split ~on:'\n'
    |> map_network_path
  in
  moves, network_map
;;

let rec calculate_ways moves network_map current_node ~(v : int) =
  match moves with
  | [] -> failwith "No path found"
  | current_move :: rest ->
    (match current_node with
     | key when equal v 1 && String.equal key "ZZZ" -> 0
     | key when equal v 2 && String.is_suffix key ~suffix:"Z" -> 0
     | key ->
       let left, right = Map.find_exn network_map key in
       (match current_move with
        | 'L' -> 1 + calculate_ways (rest @ [ current_move ]) network_map left ~v
        | 'R' -> 1 + calculate_ways (rest @ [ current_move ]) network_map right ~v
        | _ -> failwith "Invalid move"))
;;

let find_starting_points network_map =
  Map.fold network_map ~init:[] ~f:(fun ~key ~data:_ acc ->
    match key with
    | x when String.is_suffix x ~suffix:"A" -> x :: acc
    | _ -> acc)
;;

let find_node network_map key =
  match Map.find network_map key with
  | Some (left, right) -> left, right
  | None -> failwith "Node not found"
;;

let next_step current_node ~network_map ~move =
  match move with
  | 'L' -> find_node network_map current_node |> fst
  | 'R' -> find_node network_map current_node |> snd
  | _ -> failwith "Invalid move"
;;

(* Brute force method took too much hours > 15h++ *)
let rec calculate_parallel_ways ways moves network_map current_nodes =
  match moves with
  | [] -> failwith "No path found"
  | current_move :: rest ->
    if List.for_all current_nodes ~f:(String.is_suffix ~suffix:"Z")
    then ways
    else (
      let next_nodes =
        List.map current_nodes ~f:(next_step ~network_map ~move:current_move)
      in
      (calculate_parallel_ways [@tailcall])
        (ways + 1)
        (rest @ [ current_move ])
        network_map
        next_nodes)
;;

let part_one input =
  let move_list, network_map = parse_data input in
  calculate_ways move_list network_map "AAA" ~v:1
;;

let part_two input =
  let move_list, network_map = parse_data input in
  find_starting_points network_map
  |> List.map ~f:(fun s -> calculate_ways move_list network_map s ~v:2)
  |> List.fold ~init:0 ~f:(fun acc x -> if acc = 0 then x else Helper.Math.lcm acc x)
;;

let example = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"

let example_2 =
  "LR\n\n\
   11A = (11B, XXX)\n\
   11B = (XXX, 11Z)\n\
   11Z = (11B, XXX)\n\
   22A = (22B, XXX)\n\
   22B = (22C, 22C)\n\
   22C = (22Z, 22Z)\n\
   22Z = (22B, 22B)\n\
   XXX = (XXX, XXX)"
;;

module Exec = struct
  let run () =
    print_endline "Day 8:";
    let input = Helper.FileReader.read_file "../inputs/2023/day8-puzzle.txt" in
    printf "Part 1: %d\n" (part_one input);
    printf "Part 2: %d\n" (part_two input);
    print_endline "-------------------------"
  ;;
end
