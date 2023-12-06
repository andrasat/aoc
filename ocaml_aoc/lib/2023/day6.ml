open Core

let parse_data input =
  let separated_data =
    input
    |> String.split ~on:'\n'
    |> List.map ~f:(Str.split (Str.regexp " +"))
    |> List.map ~f:(List.filter ~f:(fun s -> Str.string_match (Str.regexp "[0-9]+") s 0))
    |> List.map ~f:(List.map ~f:Int.of_string)
  in
  let times = List.hd_exn separated_data in
  let distances = List.last_exn separated_data in
  List.foldi times ~init:[] ~f:(fun i acc x -> [ x; List.nth_exn distances i ] :: acc)
;;

let parse_data_v2 input =
  input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun s ->
    let trimmed = Str.global_replace (Str.regexp " +") "" s in
    match Str.search_forward (Str.regexp "[0-9]+") trimmed 0 with
    | _ -> Str.matched_string trimmed
    | exception Stdlib.Not_found -> "")
  |> List.map ~f:Int.of_string
  |> List.fold ~init:[] ~f:(fun acc x ->
    match acc with
    | [] -> [ [ x ] ]
    | hd :: tl -> if List.length hd = 2 then [ x ] :: hd :: tl else (x :: hd) :: tl)
  |> List.map ~f:List.rev
;;

(* 1 ms button hold = 1ms speed *)
let rec calculate_ways time distance hold ways =
  match time with
  | 0 -> ways
  | time_left ->
    let distance_reached = hold * time_left in
    if distance_reached > distance
    then calculate_ways (time_left - 1) distance (hold + 1) (ways + 1)
    else calculate_ways (time_left - 1) distance (hold + 1) ways
;;

let rec mul_of_ways (input : int list list) =
  match input with
  | [] -> 1
  | time_distance :: rest ->
    let time = List.hd_exn time_distance in
    let distance = List.last_exn time_distance in
    let ways = calculate_ways time distance 0 0 in
    ways * mul_of_ways rest
;;

let part_one input = parse_data input |> mul_of_ways
let part_two input = parse_data_v2 input |> mul_of_ways

module Exec = struct
  let run () =
    print_endline "Day 6:";
    let input = Helper.FileReader.read_file "../inputs/2023/day6-puzzle.txt" in
    printf "Result part 1: %d\n" (part_one input);
    printf "Result part 2: %d\n" (part_two input);
    print_endline "-------------------------"
  ;;
end
