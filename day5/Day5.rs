use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut f = File::open("input.txt").expect("boom!");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("algo cagou!");
    let vect = contents.lines()
        .map(|line| {
            line.parse::<i32>().unwrap()
        }).collect();
    let res = find_sol2(vect);
    println!("{}", res);
}

fn find_sol2(mut vect: Vec<i32>) -> i32 {
    let mut next = 0;
    let mut count = 0;
    loop {
        match vect.get_mut(next as usize) {
            Some(jump) => {
                next = *jump + next;
                if *jump >= 3 {
                    *jump = *jump - 1;
                } else {
                    *jump = *jump + 1;
                };
                count += 1;
            },
            None => return count,
        };
    }
}
