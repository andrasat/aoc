use std::collections::HashMap;

use super::readfile::read_file;

fn calculate_elves_calories(data_input: String) -> HashMap<String, i64> {
    let split = data_input.split("\n");
    let mut elves_calories: HashMap<String, i64> = HashMap::new();
    let mut total_calorie: i64 = 0;

    for data in split {
        if data != "" {
            let calorie = match data.parse::<i64>() {
                Ok(v) => v,
                Err(_) => 0,
            };
            total_calorie += calorie
        }

        if data == "" {
            let elf_count = elves_calories.len();
            let elf_number = elf_count + 1;
            let str_elf_number = elf_number.to_string();
            elves_calories.insert(str_elf_number, total_calorie);
            total_calorie = 0;
        }
    }

    return elves_calories;
}

pub fn day_1() {
    println!("\n----- DAY 1 -----");
    let path_file = String::from("../inputs/2022/day1-puzzle.txt");
    let inputs = read_file(path_file);

    let mut rank_calories: HashMap<usize, i64> = HashMap::new();

    let mut elves = calculate_elves_calories(inputs);

    while rank_calories.len() < 3 {
        let rank_count = rank_calories.len();
        let rank_number = rank_count + 1;
        let max_calorie = elves.iter().max_by(|x, y| x.1.cmp(y.1)).unwrap();

        rank_calories.insert(rank_number, max_calorie.1.to_owned());

        let elf_key = max_calorie.0.to_owned();
        elves.remove(&elf_key);
    }

    let total_calorie = rank_calories
        .iter()
        .map(|map| map.1.to_owned())
        .reduce(|sum, v| sum + v)
        .unwrap();

    println!("calories ranked: {:?}", rank_calories);
    println!("sum of 3 highest calories: {:?}", total_calorie);
}
