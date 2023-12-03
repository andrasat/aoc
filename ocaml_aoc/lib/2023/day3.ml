open Core

let match_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let match_symbol c =
  match c with
  | '.' -> false
  | '0' .. '9' -> false
  | _ -> true
;;

let generate_matrix input =
  String.split input ~on:'\n'
  |> List.map ~f:String.to_list
  |> List.to_array
  |> Array.map ~f:Array.of_list
;;

module Exec = struct
  let run () =
    print_endline "Day 3:";
    let inputs = Helper.FileReader.read_file "../inputs/2023/day3-puzzle.txt" in
    let matrix = generate_matrix inputs in
    Array.iter matrix ~f:(fun row ->
      Array.iter row ~f:(fun c -> printf "%c" c);
      print_endline "");
    let width = Array.length matrix.(0) in
    let height = Array.length matrix in
    printf "width: %d, height: %d\n" width height;
    print_endline "-------------------------"
  ;;
end
