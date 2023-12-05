open Core

let parse_data input =
  match input |> Str.split (Str.regexp "\n\n") with
  | [] -> failwith "No data"
  | seeds :: maps ->
    ( seeds
      |> Str.replace_first (Str.regexp "seeds: ") ""
      |> String.split ~on:' '
      |> List.map ~f:Int.of_string
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

let part_one ((seeds : int list), (maps : int list list list)) =
  List.fold
    ~init:0
    ~f:(fun lowest seed ->
      match lowest with
      | 0 -> find_location seed maps
      | x ->
        let loc = find_location seed maps in
        if loc < x then loc else x)
    seeds
;;

module Exec = struct
  let run () =
    print_endline "Day 5:";
    let input = Helper.FileReader.read_file "../inputs/2023/day5-puzzle.txt" in
    let seeds, maps = parse_data input in
    printf "Part 1: %d\n" (part_one (seeds, maps));
    print_endline "-------------------------"
  ;;
end
