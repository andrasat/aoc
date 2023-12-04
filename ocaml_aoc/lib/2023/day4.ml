open Core

let filter_empty_str_list str_list =
  List.filter str_list ~f:(fun s -> String.length s > 0)
;;

let calculate_card_number sum str_list =
  match str_list with
  | [] -> sum
  | _ :: [] -> sum + 1
  | _ :: rest -> sum + (1 * Int.pow 2 (List.length rest))
;;

let separate_numbers input =
  input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun s -> String.sub s ~pos:9 ~len:(String.length s - 9))
  |> List.map ~f:(fun s -> String.split s ~on:'|' |> List.map ~f:String.strip)
  |> List.map ~f:(fun sl ->
    List.map sl ~f:(String.split ~on:' ') |> List.map ~f:filter_empty_str_list)
  |> List.map ~f:(fun card_sections ->
    match card_sections with
    | [] -> []
    | _ :: [] -> []
    | winning_numbers :: card_numbers :: _ ->
      List.filter card_numbers ~f:(fun s ->
        List.exists winning_numbers ~f:(String.equal s)))
  |> List.filter ~f:(fun l -> List.length l > 0)
  |> List.fold ~init:0 ~f:calculate_card_number
;;

module Exec = struct
  let run () =
    print_endline "Day 4:";
    let input = Helper.FileReader.read_file "../inputs/2023/day4-puzzle.txt" in
    printf "Part 1: %d\n" (separate_numbers input);
    print_endline "-------------------------"
  ;;
end
