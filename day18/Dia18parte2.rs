use std::collections::VecDeque;
use std::collections::HashMap;
use std::collections::LinkedList;

enum Value {
    Pointer(char),
    Primitive(i32),
}

enum Instruction {
    Snd(char),
    Rcv(char),
    Add(char, Value),
    Mul(char, Value),
    Mod(char, Value),
    Jgz(char, Value),
}

struct Zipper {
    prev: LinkedList<Instruction>,
    curr: Instruction,
    next: LinkedList<Instruction>,
}

enum Status {
    Running,
    Waiting(char),
    Terminated,
}

struct Process {
    inq: VecDeque<i32>,
    outq: VecDeque<i32>,
    sent: i32,
    processor: HashMap<char, i32>,
    prog: Zipper,
    status: Status,
}

fn safe_move(n: i32, mut prog: Zipper) -> Option<Zipper> {
    match n {
        0 => Some(prog),
        n if n > 0 => match prog.next.pop_front() {
            None => None,
            Some(i) => {
                prog.prev.push_front(prog.curr);
                safe_move(n - 1, Zipper{
                prev: prog.prev,
                curr: i,
                next: prog.next,
            })},
        },
        n => match prog.prev.pop_front() {
            None => None,
            Some(i) => {
                prog.next.push_front(prog.curr);
                safe_move(n + 1, Zipper{
                prev: prog.prev,
                curr: i,
                next: prog.next,
            })},
        }
    }
}
