open Core

module FileReader = struct
  let read_file file_name = In_channel.create file_name |> In_channel.input_all
end

module Math = struct
  let rec gcd a b = if b = 0 then a else gcd b (a % b)
  let lcm a b = a * b / gcd a b
end
