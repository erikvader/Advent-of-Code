use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
enum Instr {
    Mask { xs: u64, rest: u64 },
    Mem { adr: u64, val: u64 },
}

impl Instr {
    fn create_mask(mask: &str) -> Self {
        let mut xs = 0;
        let mut rest = 0;

        for b in mask.chars() {
            xs <<= 1;
            rest <<= 1;

            if b == 'X' {
                xs |= 1;
            } else if b == '1' {
                rest |= 1;
            }
        }

        Self::Mask { xs, rest }
    }

    fn create_mem(adr: &str, val: &str) -> Self {
        Self::Mem {
            adr: adr.parse().unwrap(),
            val: val.parse().unwrap(),
        }
    }

    fn apply_mask(&self, n: u64) -> u64 {
        if let Instr::Mask { xs, rest } = self {
            (n & xs) | rest
        } else {
            panic!("not a mask!");
        }
    }

    fn prot2_adrs(&self, adr: u64) -> Vec<u64> {
        if let Instr::Mask { xs, rest } = *self {
            let mut adrs = Vec::new();
            let templ = (adr | rest) & !xs;

            let mut counter = 0;
            loop {
                let mut xw = xs;
                let mut cw = counter;
                let mut mask = 1;
                let mut res = templ;

                while xw > 0 {
                    if xw & 1 > 0 {
                        if cw & 1 > 0 {
                            res |= mask;
                        }
                        cw >>= 1;
                    }
                    xw >>= 1;
                    mask <<= 1;
                }

                if cw > 0 {
                    break;
                }

                adrs.push(res);
                counter += 1;
            }

            adrs
        } else {
            panic!("not a mask");
        }
    }
}

fn read_input(input: &str) -> anyhow::Result<Vec<Instr>> {
    let mask_reg = Regex::new(r"^mask = (.+)$")?;
    let mem_reg = Regex::new(r"^mem\[(.+)\] = (.+)$")?;

    let instructions = input
        .lines()
        .map(|l| {
            mask_reg
                .captures(l)
                .map(|mc| Instr::create_mask(mc.get(1).unwrap().as_str()))
                .or_else(|| {
                    mem_reg.captures(l).map(|mc| {
                        Instr::create_mem(mc.get(1).unwrap().as_str(), mc.get(2).unwrap().as_str())
                    })
                })
                .expect("no regex matched")
        })
        .collect();

    Ok(instructions)
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let mut instructions = read_input(input)?.into_iter();
    let mut mem = HashMap::new();
    let mut mask = instructions.next().unwrap();

    for i in instructions {
        if let Instr::Mem { adr, val } = i {
            mem.insert(adr, mask.apply_mask(val));
        } else {
            mask = i;
        }
    }

    let sum: u64 = mem.into_iter().map(|(_, v)| v).sum();

    Ok(sum.to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let mut instructions = read_input(input)?.into_iter();
    let mut mem = HashMap::new();
    let mut mask = instructions.next().unwrap();

    for i in instructions {
        if let Instr::Mem { adr, val } = i {
            for a in mask.prot2_adrs(adr) {
                mem.insert(a, val);
            }
        } else {
            mask = i;
        }
    }

    let sum: u64 = mem.into_iter().map(|(_, v)| v).sum();

    Ok(sum.to_string())
}
