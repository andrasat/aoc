open Core

let card_strength_v1 = function
  | 'A' -> 13
  | 'K' -> 12
  | 'Q' -> 11
  | 'J' -> 10
  | 'T' -> 9
  | '9' -> 8
  | '8' -> 7
  | '7' -> 6
  | '6' -> 5
  | '5' -> 4
  | '4' -> 3
  | '3' -> 2
  | '2' -> 1
  | _ -> failwith "Invalid card"
;;

let card_strength_v2 = function
  | 'A' -> 13
  | 'K' -> 12
  | 'Q' -> 11
  | 'T' -> 10
  | '9' -> 9
  | '8' -> 8
  | '7' -> 7
  | '6' -> 6
  | '5' -> 5
  | '4' -> 4
  | '3' -> 3
  | '2' -> 2
  | 'J' -> 1
  | _ -> failwith "Invalid card"
;;

let set_card_strength_v = function
  | 1 -> card_strength_v1
  | 2 -> card_strength_v2
  | _ -> failwith "Invalid version"
;;

let hand_strength = function
  | (_, n) :: _ when n = 5 -> 7
  | (_, n) :: _ when n = 4 -> 6
  | [ (_, n); (_, n') ] when n = 3 && n' = 2 -> 5
  | (_, n) :: (_, n') :: _ when n = 3 && n' = 1 -> 4
  | (_, n) :: (_, n') :: _ when n = 2 && n' = 2 -> 3
  | (_, n) :: (_, n') :: _ when n = 2 && n' = 1 -> 2
  | _ -> 1
;;

let format_hand hand =
  match hand with
  | [ cards; bid ] -> cards, bid
  | _ -> failwith "Invalid input"
;;

let parse_data input =
  input
  |> String.split_lines
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:format_hand
;;

let card_checker (c, _) ~comparator = Char.equal c comparator
let card_checker_inverse (c, _) ~comparator = not (Char.equal c comparator)

let get_hand_type hand =
  String.to_list hand
  |> List.fold ~init:[] ~f:(fun acc c ->
    match acc with
    | [] -> [ c, 1 ]
    | (c', n) :: [] when Char.equal c c' -> [ c', n + 1 ]
    | rest ->
      (match List.find rest ~f:(card_checker ~comparator:c) with
       | Some (c'', n') ->
         (c'', n' + 1) :: List.filter rest ~f:(card_checker_inverse ~comparator:c)
       | None -> (c, 1) :: rest))
  |> List.sort ~compare:(fun (_, n) (_, n') -> Int.compare n' n)
;;

let get_hand_type_v2 hand =
  get_hand_type hand
  |> List.fold ~init:[] ~f:(fun acc (c, n) ->
    match acc with
    | [] -> [ c, n ]
    | (c', n') :: rest when Char.equal c' 'J' -> (c, n' + n) :: rest
    | (c', n') :: rest ->
      (match c with
       | 'J' -> (c', n' + n) :: rest
       | _ -> acc @ [ c, n ]))
;;

let set_f_hand_type_v = function
  | 1 -> get_hand_type
  | 2 -> get_hand_type_v2
  | _ -> failwith "Invalid version"
;;

let rec first_card_compare hand1 hand2 ~(card_strength_version : char -> int) =
  let hand1_list = String.to_list hand1 in
  let hand2_list = String.to_list hand2 in
  match hand1_list, hand2_list with
  | [], [] -> failwith "Invalid input"
  | first_card1 :: card_rest1, first_card2 :: card_rest2 ->
    let card1 = card_strength_version first_card1 in
    let card2 = card_strength_version first_card2 in
    if card1 = card2
    then
      first_card_compare
        (String.of_char_list card_rest1)
        (String.of_char_list card_rest2)
        ~card_strength_version
    else Int.compare card2 card1
  | _ -> failwith "Invalid input"
;;

let hand_compare (hand1, _) (hand2, _) ~(v : int) =
  let h1 = set_f_hand_type_v v hand1 |> hand_strength in
  let h2 = set_f_hand_type_v v hand2 |> hand_strength in
  if h1 = h2
  then first_card_compare hand1 hand2 ~card_strength_version:(set_card_strength_v v)
  else Int.compare h2 h1
;;

let rec sum_of_bid (hand : (string * string) list) n =
  match hand with
  | [] -> 0
  | (_, bid) :: rest -> (Int.of_string bid * n) + sum_of_bid rest (n - 1)
;;

let part_one input =
  let parsed = parse_data input |> List.sort ~compare:(hand_compare ~v:1) in
  sum_of_bid parsed (List.length parsed)
;;

let part_two input =
  let parsed = parse_data input |> List.sort ~compare:(hand_compare ~v:2) in
  sum_of_bid parsed (List.length parsed)
;;

let example = "289AJ 592\n275TQ 606\n2685Q 210"

module Exec = struct
  let run () =
    print_endline "Day 7:";
    let input = Helper.FileReader.read_file "../inputs/2023/day7-puzzle.txt" in
    printf "Result part 1: %d\n" (part_one input);
    printf "Result part 2: %d\n" (part_two input);
    print_endline "-------------------------"
  ;;
end
