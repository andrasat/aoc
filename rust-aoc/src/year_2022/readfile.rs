use std::fs;

pub fn read_file(path: String) -> String {
    let err_msg = format!("Cannot open file {}", path);
    let file_contents = fs::read_to_string(path).expect(&err_msg);
    return file_contents;
}
