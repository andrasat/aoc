module Utils = struct
  let rec string_joiner sep = function
    | [] -> ""
    | [ x ] -> x
    | x :: xs -> x ^ sep ^ string_joiner sep xs
  ;;

  let rec get_last_element (ls : 'a list) =
    match ls with
    | [] -> failwith "Empty list"
    | [ x ] -> x
    | _ :: xs -> get_last_element xs
  ;;
end

module FileReader = struct
  let read_file file_name =
    let file = open_in file_name in
    let rec read_lines lines =
      try
        let line = input_line file in
        read_lines (lines ^ line ^ "\n")
      with
      | End_of_file ->
        close_in file;
        let n = String.length lines in
        String.sub lines 0 (n - 1)
    in
    read_lines ""
  ;;
end
