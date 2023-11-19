module Utils = struct
  let rec string_joiner sep = function
    | [] -> ""
    | [ x ] -> x
    | x :: xs -> x ^ sep ^ string_joiner sep xs
  ;;
end

module FileReader = struct
  let read_file file_name =
    let file = open_in file_name in
    let rec read_lines lines =
      try
        let line = input_line file in
        read_lines (line :: lines)
      with
      | End_of_file ->
        close_in file;
        List.rev lines
    in
    read_lines []
  ;;
end
