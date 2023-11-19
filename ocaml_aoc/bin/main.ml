let () =
  let print_elems elem = Printf.printf "Data: %s\n" elem in
  List.iter print_elems (Helper.FileReader.read_file "../inputs/day4-2022-example.txt")
;;
