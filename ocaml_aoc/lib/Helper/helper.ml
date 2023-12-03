open Core

module FileReader = struct
  let read_file file_name = In_channel.create file_name |> In_channel.input_all
end
