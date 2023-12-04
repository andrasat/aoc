open Core

let filter_empty_str_list str_list =
  List.filter str_list ~f:(fun s -> String.length s > 0)
;;

let extract_card_numbers str_list =
  str_list |> List.map ~f:(String.split ~on:' ') |> List.map ~f:filter_empty_str_list
;;

let filter_winning_numbers card_sections =
  match card_sections with
  | [] -> []
  | _ :: [] -> []
  | winning_numbers :: card_numbers :: _ ->
    List.filter card_numbers ~f:(fun s -> List.exists winning_numbers ~f:(String.equal s))
;;

let parse_data input =
  input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun s -> String.sub s ~pos:9 ~len:(String.length s - 9))
  |> List.map ~f:(fun s -> String.split s ~on:'|' |> List.map ~f:String.strip)
  |> List.map ~f:extract_card_numbers
  |> List.map ~f:filter_winning_numbers
;;

let calculate_card_number sum str_list =
  match str_list with
  | [] -> sum
  | _ :: [] -> sum + 1
  | _ :: rest -> sum + Int.pow 2 (List.length rest)
;;

let part_one input =
  parse_data input
  |> List.filter ~f:(fun l -> List.length l > 0)
  |> List.fold ~init:0 ~f:calculate_card_number
;;

let cards_to_add next_card_copies =
  next_card_copies
  |> List.fold ~init:(0, []) ~f:(fun (sum, new_next_card_copies) card_copies ->
    match card_copies with
    | [] -> sum, new_next_card_copies
    | copy_num :: copy_rest -> sum + copy_num, copy_rest :: new_next_card_copies)
;;

let rec calculate_scratch_cards
  (winning_numbers : string list list)
  ~(next_card_copies : int list list)
  ~scratch_cards
  =
  match winning_numbers with
  | [] -> scratch_cards
  | num_list :: rest ->
    let card_copies, new_next_card_copies = cards_to_add next_card_copies in
    let card_sum = 1 + card_copies in
    if List.length num_list > 0
    then (
      let additional_card_copies =
        List.init (List.length num_list) ~f:(fun _ -> card_sum)
      in
      calculate_scratch_cards
        rest
        ~next_card_copies:(additional_card_copies :: new_next_card_copies)
        ~scratch_cards:(card_sum :: scratch_cards))
    else
      calculate_scratch_cards
        rest
        ~next_card_copies:new_next_card_copies
        ~scratch_cards:(card_sum :: scratch_cards)
;;

let part_two input =
  parse_data input
  |> calculate_scratch_cards ~next_card_copies:[] ~scratch_cards:[]
  |> List.fold ~init:0 ~f:( + )
;;

module Exec = struct
  let run () =
    print_endline "Day 4:";
    let input = Helper.FileReader.read_file "../inputs/2023/day4-puzzle.txt" in
    printf "Part 1: %d\n" (part_one input);
    printf "Part 2: %d\n" (part_two input);
    print_endline "-------------------------"
  ;;
end
