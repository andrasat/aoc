open Core

type symbol_data =
  { value : char
  ; row : int
  ; col : int
  }

type number_data =
  { value : int
  ; row : int
  ; col_min : int
  ; col_max : int
  }

let check_symbol c =
  match c with
  | '.' -> false
  | '0' .. '9' -> false
  | _ -> true
;;

let check_neighbour (num : number_data) (sym : symbol_data) =
  abs (num.row - sym.row) <= 1 && num.col_min - 1 <= sym.col && sym.col <= num.col_max + 1
;;

let generate_matrix input = String.split input ~on:'\n' |> List.map ~f:String.to_list

let rec extract_row
  (row : char list)
  num_list
  r
  c
  (number_list : number_data list)
  (symbol_list : symbol_data list)
  =
  match row with
  | [] -> number_list, symbol_list
  | ('0' .. '9' as c1) :: ('0' .. '9' as c2) :: rest ->
    extract_row (c2 :: rest) (c1 :: num_list) r (c + 1) number_list symbol_list
  | ('0' .. '9' as c1) :: rest ->
    let value = c1 :: num_list |> List.rev |> String.of_char_list |> Int.of_string in
    let num_data = { value; row = r; col_min = c - List.length num_list; col_max = c } in
    extract_row rest [] r (c + 1) (num_data :: number_list) symbol_list
  | ch :: rest ->
    if check_symbol ch
    then (
      let symbol_data = { value = ch; row = r; col = c } in
      extract_row rest [] r (c + 1) number_list (symbol_data :: symbol_list))
    else extract_row rest [] r (c + 1) number_list symbol_list
;;

let rec extract_values
  (matrix : char list list)
  ?(r = 0)
  (number_list : number_data list)
  (symbol_list : symbol_data list)
  =
  match matrix with
  | [] -> number_list, symbol_list
  | row :: rest ->
    let number_list, symbol_list = extract_row row [] r 0 number_list symbol_list in
    extract_values rest ~r:(r + 1) number_list symbol_list
;;

let part_one matrix =
  let number_list, symbol_list = extract_values matrix [] [] in
  number_list
  |> List.filter ~f:(fun num ->
    symbol_list |> List.exists ~f:(fun sym -> check_neighbour num sym))
  |> List.map ~f:(fun num -> num.value)
  |> List.fold ~init:0 ~f:(fun sum num -> num + sum)
;;

let part_two matrix =
  let number_list, symbol_list = extract_values matrix [] [] in
  symbol_list
  |> List.filter ~f:(fun (sym : symbol_data) -> Char.equal sym.value '*')
  |> List.fold ~init:0 ~f:(fun gear_ratio sym ->
    let neighborhood_number_list =
      number_list |> List.filter ~f:(fun num -> check_neighbour num sym)
    in
    match neighborhood_number_list with
    | [ { value = v1; _ }; { value = v2; _ } ] -> (v1 * v2) + gear_ratio
    | _ -> gear_ratio)
;;

module Exec = struct
  let run () =
    print_endline "Day 3:";
    let inputs = Helper.FileReader.read_file "../inputs/2023/day3-puzzle.txt" in
    let matrix = generate_matrix inputs in
    printf "Part 1: %d\n" (part_one matrix);
    printf "Part 2: %d\n" (part_two matrix);
    print_endline "-------------------------"
  ;;
end
