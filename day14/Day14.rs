mod day10;

use std::collections::VecDeque;

fn count_ones(v: &Vec<usize>) -> i32 {
    v.iter()
        .fold(0, |acc, &x| acc + to_bin(&x).iter().sum::<i32>())
}

fn to_bin(x: &usize) -> VecDeque<i32> {
    let pad = |mut bs: VecDeque<_>| {
        let missing = std::cmp::max(0, 8 - bs.len());
        for _ in 0..missing {
            bs.push_back(0);
        }
        bs
    };

    fn go(x: &usize, mut acc: VecDeque<i32>) -> VecDeque<i32> {
        if *x <= 0 {
            acc
        } else {
            if x % 2 == 0 {
                acc.push_front(0);
                let next = x / 2;
                go(&next, acc)
            } else {
                acc.push_front(1);
                let next = x / 2;
                go(&next, acc)
            }
        }
    }
    go(x, VecDeque::new())
}

fn make_grid(input: String) -> Vec<Vec<usize>> {
    let mut output = vec![];
    for i in 0..128 {
        let s = input.clone() + "-" + &i.to_string();
        // println!("{}", s);
        output.push(day10::hash(&s).value);
    }
    output
}

fn main() {
    let input = String::from("stpzcrnm");
    // let hash = day10::hash(&input);
    // println!("{:?}", hash);
    // println!("{:?}", hash.to_hex());

    println!("Começando a criar grade...");
    let grid = make_grid(input);
    println!("Grade construída.");

    // println!("{:?}", (&grid[0]).into_iter().map(to_bin).collect::<VecDeque<_>>());

    let mut count = 0;
    for line in &grid {
        count += count_ones(line);
    }
    println!("Ones: {}", count);
}
