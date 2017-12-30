const STEPS: i32 = 363;

fn main() {
    let mut size = 1;
    let mut pos = 0;
    let mut acc = 0;
    for _ in 0..50_000_000 {
        pos = 1 + ((pos + STEPS) % size);
        acc = if pos == 1 {
            size
        } else {
            acc
        };
        size += 1;
    }
    println!("{}", acc);
}
