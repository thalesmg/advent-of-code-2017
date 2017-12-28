mod day10;

extern crate ordermap;

use std::collections::VecDeque;
use std::collections::HashSet;
use std::collections::HashMap;
use ordermap::set::OrderSet;

fn count_ones(v: &Vec<usize>) -> i32 {
    v.iter()
        .fold(0, |acc, &x| acc + to_bin(&x).iter().sum::<i32>())
}

fn to_bin(x: &usize) -> VecDeque<i32> {
    let pad = |mut bs: VecDeque<_>| {
        let missing = std::cmp::max(0, 8 - bs.len());
        for _ in 0..missing {
            bs.push_front(0);
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
    pad(go(x, VecDeque::new()))
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

type Coord = (usize, usize);

fn to_bin_grid(grid: Vec<Vec<usize>>) -> OrderSet<Coord> {
    let mut output = OrderSet::new();

    for (i, line) in grid.into_iter().enumerate() {
        let binline = line.iter()
            .flat_map(to_bin);
        for (j, num) in binline.into_iter().enumerate() {
            if num == 1 {
                output.insert((i, j));
            }
        }
    }

    output
}

fn make_component_map(mut set: OrderSet<Coord>) -> HashMap<i32, OrderSet<Coord>> {
    fn find_neighbors(xy: Coord, set: &mut OrderSet<Coord>, acc: &mut OrderSet<Coord>) -> OrderSet<Coord> {
        let (x, y) = xy;
        let x = x as i16;
        let y = y as i16;

        let nexts = ((x - 1)..(x + 1 + 1))
            .flat_map(|x_| {
                ((y - 1)..(y + 1 + 1))
                    .filter(move |y_| (x_ + y_ - x - y).abs() == 1)
                    .map(move |y_| (x_, y_))
                    .collect::<Vec<_>>()
            });

        // xs.zip(ys)
        //     .filter(|&(x_, y_)| (y_ + x_ - x - y).abs() == 1)
        //     .filter(|&(x_, y_)| !acc.contains(&(x_ as usize, y_ as usize)))
        //     .for_each(|(x_, y_)| {
        //         let x_ = x_ as usize;
        //         let y_ = y_ as usize;
        //         if let Some(_) = set.take(&(x_, y_)) {
        //             acc.insert((x_, y_));
        //             acc.union(&find_neighbors((x_, y_), set, acc));
        //         }
        //     });

        let acc = nexts.fold(acc.clone(), |mut acc, (x_, y_)| {
            let x_ = x_ as usize;
            let y_ = y_ as usize;
            if let Some(_) = set.take(&(x_, y_)) {
                acc.insert((x_, y_));
                let ns_ = find_neighbors((x_, y_), set, &mut acc);
                acc.union(&ns_);
            };
            acc
        });

        acc
    }

    let mut ind = 0;
    let mut comp_map = HashMap::new();

    while !set.is_empty() {
        if let Some(el) = set.pop() {
            let components = find_neighbors(el, &mut set, &mut OrderSet::new());
            // println!("comp len {}", components.len());
            comp_map.insert(ind, components);
            // println!("antes {}", set.len());
            let current = &comp_map[&ind];
            // println!("current {:?}", current);
            set.retain(|&x| !current.contains(&x));
            // println!("depois {}", set.len());
        };
        ind += 1;
    }

    comp_map
}

fn main() {
    let input = String::from("stpzcrnm");

    // let (x, y): (i16, i16) = (2, 3);
    // let boom = ((x - 1)..(x + 1 + 1))
    //     .flat_map(|x_| {
    //         ((y - 1)..(y + 1 + 1))
    //             .filter(move |y_| (x_ + y_ - x - y).abs() == 1)
    //             .map(move |y_| (x_, y_))
    //             .collect::<Vec<_>>()
    //     })
    //     .collect::<Vec<_>>();
    // println!("{:?}", boom);

    println!("Começando a criar grade...");
    let grid = make_grid(input);
    println!("Grade construída.");

    // let mut count = 0;
    // for line in &grid {
    //     count += count_ones(line);
    // }
    // println!("Ones: {}", count);

    let bin_grid = to_bin_grid(grid);
    let comp_map = make_component_map(bin_grid);
    println!("Número de regiões: {}", comp_map.keys().count());
}
