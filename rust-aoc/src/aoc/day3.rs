use super::readfile::read_file;

pub fn day_3() {
    println!("\n----- DAY 3 -----");
    let path_file = String::from("../inputs/day3-puzzle.txt");
    let inputs = read_file(path_file);

    let split_inputs = inputs.split("\n");

    let mut rearrangement_vec: Vec<u8> = Vec::new();

    for row in split_inputs {
        let median = row.len() / 2;
        let first_compartment = &row[..median];
        let second_compartment = &row[median..];

        let first_comp_chars: Vec<char> = first_compartment.chars().collect();

        for first_comp_char in first_comp_chars {
            if second_compartment.contains(first_comp_char) {
                let ascii_num = first_comp_char as u8;
                if ascii_num >= 97 {
                    rearrangement_vec.push(ascii_num - 96);
                    break;
                } else if ascii_num >= 65 {
                    rearrangement_vec.push(ascii_num - 38);
                    break;
                }
            }
        }
    }

    let sum: u64 = match rearrangement_vec
        .iter()
        .map(|v| v.to_owned() as u64)
        .reduce(|sum: u64, v: u64| sum + v)
    {
        Some(v) => v,
        None => 0,
    };

    println!("rearrangement sum: {}", sum);

    println!("\n----- DAY 3: Part Two -----");

    let cloned_inputs = inputs.clone();
    let split_input_two = cloned_inputs.split("\n");

    let mut all_group_vec: Vec<u8> = Vec::new();

    let mut first_rucksack_group = "";
    let mut second_rucksack_group = "";
    let mut third_rucksack_group = "";

    let count: u16 = 0;
    for row in split_input_two {
        count += 1;

        // let first_comp_chars: Vec<char> = first_compartment.chars().collect();

        // for first_comp_char in first_comp_chars {
        //     if second_compartment.contains(first_comp_char) {
        //         let ascii_num = first_comp_char as u8;
        //         if ascii_num >= 97 {
        //             rearrangement_vec.push(ascii_num - 96);
        //             break;
        //         } else if ascii_num >= 65 {
        //             rearrangement_vec.push(ascii_num - 38);
        //             break;
        //         }
        //     }
        // }
    }
}
