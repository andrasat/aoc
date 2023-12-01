let digit_only_regexp = Str.regexp "[1-9]"

let digit_and_wordy_digit_regexp =
  Str.regexp "[1-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"
;;

let word_to_digit word =
  match word with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | x -> x
;;

let get_first_digit input regx =
  match Str.search_forward regx input 0 with
  | _ -> word_to_digit (Str.matched_string input)
  | exception Not_found -> ""
;;

let get_last_digit input regx =
  match Str.search_backward regx input (String.length input - 1) with
  | _ -> word_to_digit (Str.matched_string input)
  | exception Not_found -> ""
;;

let string_to_int input regx =
  let first_digit, last_digit = get_first_digit input regx, get_last_digit input regx in
  int_of_string (first_digit ^ last_digit)
;;

let rec sum_of_ints_part_one input_list =
  match input_list with
  | [] -> 0
  | x :: xs -> string_to_int x digit_only_regexp + sum_of_ints_part_one xs
;;

let rec sum_of_ints_part_two input_list =
  match input_list with
  | [] -> 0
  | x :: xs -> string_to_int x digit_and_wordy_digit_regexp + sum_of_ints_part_two xs
;;

module Exec = struct
  let run () =
    print_endline "Day 1: ";
    let inputs = Helper.FileReader.read_file "../inputs/2023/day1-puzzle.txt" in
    let input_list = String.split_on_char '\n' inputs in
    Printf.printf "Part 1: %d \n" (sum_of_ints_part_one input_list);
    Printf.printf "Part 2: %d \n" (sum_of_ints_part_two input_list);
    print_endline "-------------------------"
  ;;
end
