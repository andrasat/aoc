open Core

let parse_seeds_normal seeds =
  seeds
  |> Str.replace_first (Str.regexp "seeds: ") ""
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
;;

let rec generate_seeds_range (seeds_list : string list) =
  match seeds_list with
  | [] -> []
  | [ _ ] -> failwith "Invalid seeds"
  | start :: length :: rest -> [ start; length ] :: generate_seeds_range rest
;;

let parse_seeds_range seeds =
  seeds
  |> Str.replace_first (Str.regexp "seeds: ") ""
  |> String.split ~on:' '
  |> generate_seeds_range
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let parse_data input ~seed_parser =
  match input |> Str.split (Str.regexp "\n\n") with
  | [] -> failwith "No data"
  | seeds :: maps ->
    ( seed_parser seeds
    , maps
      |> List.map ~f:(String.split ~on:'\n')
      |> List.map ~f:List.tl_exn
      |> List.map ~f:(List.map ~f:(String.split ~on:' '))
      |> List.map ~f:(List.map ~f:(List.map ~f:Int.of_string)) )
;;

let rec find_location loc (maps : int list list list) =
  match maps with
  | [] -> loc
  | each_map :: rest_of_maps ->
    (match each_map with
     | [] -> find_location loc rest_of_maps
     | each_row :: rest_of_rows ->
       (match each_row with
        | [] -> failwith "Invalid map"
        | [ dest; src; length ] ->
          let max_src = src + length in
          if loc >= src && loc <= max_src
          then (
            let new_loc = loc + (dest - src) in
            find_location new_loc rest_of_maps)
          else find_location loc (rest_of_rows :: rest_of_maps)
        | _ -> failwith "Invalid map"))
;;

let find_lowest_location ((seeds : int list), (maps : int list list list)) =
  List.fold
    ~init:0
    ~f:(fun lowest seed ->
      let new_loc = find_location seed maps in
      if lowest = 0 || new_loc < lowest then new_loc else lowest)
    seeds
;;

let part_one input =
  let seeds, maps = parse_data input ~seed_parser:parse_seeds_normal in
  find_lowest_location (seeds, maps)
;;

let find_lowest_location_v2 ((seeds : int list list), (maps : int list list list)) =
  List.fold
    ~init:0
    ~f:(fun lowest seed_range ->
      let seed_start = List.hd_exn seed_range in
      let seed_length = List.nth_exn seed_range 1 in
      let range = List.init seed_length ~f:(fun i -> seed_start + i) in
      let new_loc = find_lowest_location (range, maps) in
      if lowest = 0 || new_loc < lowest then new_loc else lowest)
    seeds
;;

let part_two input =
  let seeds, maps = parse_data input ~seed_parser:parse_seeds_range in
  find_lowest_location_v2 (seeds, maps)
;;

module Exec = struct
  let run () =
    print_endline "Day 5:";
    let input = Helper.FileReader.read_file "../inputs/2023/day5-puzzle.txt" in
    printf "Part 1: %d\n" (part_one input);
    printf "Part 2: %d\n" (part_two input);
    print_endline "-------------------------"
  ;;
end
