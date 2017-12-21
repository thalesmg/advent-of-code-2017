mod day10;

fn hex2bin(ch: Char) -> Vec<bool> {

}

fn main() {
    let input = String::from("stpzcrnm");
    let hash = day10::hash(input);
    println!("{}", hash);
}
