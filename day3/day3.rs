use std::collections::HashMap;

type Coord = (i32, i32);

fn neighbors(pos: Coord) -> Vec<Coord> {
    let (x, y) = pos;
    vec![
        (x + 1, y),
        (x + 1, y + 1),
        (x, y + 1),
        (x - 1, y + 1),
        (x - 1, y),
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1)
        ]
}

fn sum_neighbors(spiral: &HashMap<Coord, i32>, pos: Coord) -> i32 {
    let mut acc = 0;
    for neighbor in neighbors(pos) {
        match spiral.get(&neighbor) {
            None => continue,
            Some(x) => acc = acc + x,
        }
    }

    acc
}

fn up(pos: &Coord) -> Coord {
    let &(x, y) = pos;
    (x, y + 1)
}

fn down(pos: &Coord) -> Coord {
    let &(x, y) = pos;
    (x, y - 1)
}

fn left(pos: &Coord) -> Coord {
    let &(x, y) = pos;
    (x - 1, y)
}

fn right(pos: &Coord) -> Coord {
    let &(x, y) = pos;
    (x + 1, y)
}

fn next_pos(spiral: &HashMap<Coord, i32>, last_pos: Coord, cur_pos: Coord) -> Coord {
    let (x0, y0) = last_pos;
    let (x1, y1) = cur_pos;

    match (x1 - x0, y1 - y0) {
        (0, 1) =>
            match spiral.get(&left(&cur_pos)) {
                Some(_) => up(&cur_pos),
                None => left(&cur_pos),
            },

        (0, -1) =>
            match spiral.get(&right(&cur_pos)) {
                Some(_) => down(&cur_pos),
                None => right(&cur_pos),
            },

        (1, 0) =>
            match spiral.get(&up(&cur_pos)) {
                Some(_) => right(&cur_pos),
                None => up(&cur_pos),
            },

        (-1, 0) =>
            match spiral.get(&down(&cur_pos)) {
                Some(_) => left(&cur_pos),
                None => down(&cur_pos),
            },

        error => panic!("fuck! {:?}", error),
    }
}

fn main() {
    let mut spiral = HashMap::new();
    spiral.insert((0, 0), 1);
    let mut last = 0;
    let mut last_pos = (0, 0);
    let mut cur_pos = (1, 0);
    let target = 265149;
    // let target = 806;

    loop {
        if last > target {
            println!("{}", last);
            break;
        }

        last = sum_neighbors(&spiral, cur_pos);
        spiral.insert(cur_pos, last);
        let next = next_pos(&spiral, last_pos, cur_pos);
        last_pos = cur_pos;
        cur_pos = next;
        println!("last: {}; cur_pos: {:?}; last_pos: {:?}", last, cur_pos, last_pos);
    }
}
