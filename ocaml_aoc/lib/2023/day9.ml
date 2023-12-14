open Core

let example = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

let parse_data input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string)
;;

let rec predict_next_value (history : int list) ~num =
  match history with
  | last :: [] -> last + num
  | first :: second :: rest -> predict_next_value (second :: rest) ~num:(second - first)
  | _ -> failwith "Invalid history"
;;

let rec sum_values (histories : int list list) ~sum =
  match histories with
  | [] -> sum
  | history :: rest -> sum_values rest ~sum:(sum + predict_next_value history ~num:0)
;;

let part_one input = input |> parse_data |> sum_values ~sum:0

module Exec = struct
  let run () =
    print_endline "Day 9:";
    let _ = Helper.FileReader.read_file "../inputs/2023/day9-puzzle.txt" in
    printf "Part 1: %d\n" (part_one example);
    print_endline "-------------------------"
  ;;
end
