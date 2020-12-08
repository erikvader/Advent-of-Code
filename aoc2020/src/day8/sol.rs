use anyhow;

#[derive(Debug)]
enum EndState<T> {
    Looping(T),
    End(T),
    Error,
}

fn execute(program: &Vec<(&str, i32)>) -> EndState<i32> {
    let mut acc = 0;
    let mut pc: i32 = 0;
    let mut visited: Vec<bool> = program.iter().map(|_| false).collect();

    loop {
        if visited[pc as usize] {
            break EndState::Looping(acc);
        }
        visited[pc as usize] = true;

        let (op, int) = program[pc as usize];
        match op {
            "nop" => (),
            "jmp" => {
                pc += int;
                pc -= 1;
            }
            "acc" => {
                acc += int;
            }
            "end" => break EndState::End(acc),
            _ => break EndState::Error,
        }
        pc += 1;
    }
}

fn read_prog(input: &str) -> Vec<(&str, i32)> {
    input
        .lines()
        .map(|ins| {
            let mut parts = ins.split(" ");
            // TODO: don't just expect these
            let opcode = parts.next().expect("no opcode");
            let int = parts
                .next()
                .expect("no int argument")
                .parse::<i32>()
                .expect("couldn't parse int argument");
            (opcode, int)
        })
        .chain(std::iter::once(("end", 0)))
        .collect::<Vec<_>>()
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let program = read_prog(input);
    let state = execute(&program);
    if let EndState::Looping(acc) = state {
        return Ok(acc.to_string());
    } else {
        anyhow::bail!("comput0r is not looping");
    }
}

fn swap(program: &mut Vec<(&str, i32)>, pc: usize) {
    match program[pc].0 {
        "jmp" => program[pc].0 = "nop",
        "nop" => program[pc].0 = "jmp",
        _ => panic!("??????"),
    }
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let mut program = read_prog(input);

    for i in 0..program.len() - 1 {
        if program[i].0 == "acc" {
            continue;
        }

        swap(&mut program, i);
        if let EndState::End(acc) = execute(&program) {
            return Ok(acc.to_string());
        }
        swap(&mut program, i);
    }

    anyhow::bail!("nothing worked?");
}
