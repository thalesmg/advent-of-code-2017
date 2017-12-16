use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;

fn twist(ribbon: &mut HashMap<usize, usize>, cur_index: usize, length: usize) {
    let mut indices = gen_indices(&ribbon, cur_index, length);

    while indices.len() > 0 {
        let swap_indices = indices
            .pop_front()
            .and_then(|one| {
                indices
                    .pop_back()
                    .and_then(|two| {
                        Some((one, two))
                    })
            });
        match swap_indices {
            Some((one, two)) => {
                let vone = ribbon[&one];
                let vtwo = ribbon[&two];
                ribbon.insert(one, vtwo);
                ribbon.insert(two, vone);
            },
            None => break,
        }
    }
}

fn gen_indices(ribbon: &HashMap<usize, usize>, cur_index: usize, length: usize) -> VecDeque<usize> {
    let size = ribbon.len();
    let mut indices: VecDeque<usize> = VecDeque::new();
    for i in cur_index..(cur_index + length) {
        indices.push_back(i % size);
    }
    indices
}

fn test() {
    let lens = vec![3,4,1,5];
    let mut ribbon = HashMap::new();
    let mut cur_index = 0;
    let mut skip_size = 0;
    for i in 0..5 {
        ribbon.insert(i, i);
    }
    let size = ribbon.len();
    for len in lens {
        twist(&mut ribbon, cur_index, len);
        cur_index = (cur_index + len + skip_size) % size;
        skip_size += 1;
    }
    println!("ribbon: {:?}", ribbon);
}

fn part1() {
    let mut f = File::open("input.txt").expect("bum!");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("algo cagou!");
    let lens = contents
        .split(",")
        .map(|num| {
            num.trim().parse::<usize>().unwrap()
        });
    let mut ribbon = HashMap::new();
    let mut cur_index = 0;
    let mut skip_size = 0;
    for i in 0..256 {
        ribbon.insert(i, i);
    }
    let size = ribbon.len();
    for len in lens {
        twist(&mut ribbon, cur_index, len);
        cur_index = (cur_index + len + skip_size) % size;
        skip_size += 1;
    }
    println!("ribbon: {:?}", ribbon);
    println!("{}", ribbon[&(0 as usize)] * ribbon[&(1 as usize)]);
}

fn main() {
    part1();
}
