module Crate = struct
  type t = Stack of char

  let empty = Stack.Empty
  let compare = compare
end

module MapOfCrate = Map.Make (Int)

let map = MapOfCrate.empty

let rec insert_crates (input_list : int option list) (map_input : 'MapOfCrate) =
  match input_list with
  | [] -> map_input
  | x :: xs ->
    (match x with
     | None -> insert_crates xs map_input
     | Some v ->
       let empty_crate = Stack.create () in
       let new_map = MapOfCrate.add v empty_crate map_input in
       insert_crates xs new_map)
;;

let generate_crates (inputs : string list) =
  let list_of_int_opt =
    List.nth inputs 6 |> String.split_on_char ' ' |> List.map int_of_string_opt
  in
  List.iter
    (fun x ->
      match x with
      | None -> print_string "None,"
      | Some v -> Printf.printf "%d," v)
    list_of_int_opt;
  print_endline "";
  insert_crates list_of_int_opt map
;;

module Exec = struct
  let run () =
    print_endline "Day 5: ";
    let _ = Helper.FileReader.read_file "../inputs/2022/day5-puzzle.txt" in
    ()
  ;;
end
