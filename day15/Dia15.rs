struct Generator {
    factor: i32,
    divisor: i32,
    previous: i32,
    mask: i32,
}

impl Generator {
    fn produce(&mut self) -> i32 {
        let result = (self.previous as i64 * self.factor as i64) % self.divisor as i64;
        self.previous = result as i32;
        self.previous
    }

    fn produce2(&mut self) -> i32 {
        let val = self.produce();
        if val & self.mask == 0 {
            val
        } else {
            self.produce2()
        }
    }
}

fn judge(a: i32, b: i32) -> bool {
    ((a ^ b) & 0xffff) == 0
}

fn main() {
    let mut gen_a = Generator{
        factor: 16807,
        divisor: 2147483647,
        // previous: 65,
        previous: 516,
        mask: (1 << 0) | (1 << 1),
    };
    let mut gen_b = Generator{
        factor: 48271,
        divisor: 2147483647,
        // previous: 8921,
        previous: 190,
        mask: (1 << 0) | (1 << 1) | (1 << 2),
    };

    let mut count = 0;
    let mut res_a;
    let mut res_b;

    // for _ in 0..40_000_000 {
    //     res_a = gen_a.produce();
    //     res_b = gen_b.produce();
    //     // println!("{:32b}", res_a);
    //     // println!("{:32b}", res_b);
    //     // println!("{}", (res_a ^ res_b) & 0xffff);
    //     // println!("{}", judge(res_a, res_b));
    //     // println!("");
    //     if judge(res_a, res_b) {
    //         count += 1;
    //     }
    // }
    for _ in 0..5_000_000 {
        res_a = gen_a.produce2();
        res_b = gen_b.produce2();
        if judge(res_a, res_b) {
            count += 1;
        }
    }
    println!("{}", count);
}
