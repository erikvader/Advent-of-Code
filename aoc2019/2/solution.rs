use std::fs;

fn opcode<F>(progmem: &mut Vec<u32>, cursor: &mut usize, f: F) -> Option<()>
   where F: Fn(u32, u32) -> u32
{
   let op1 = *progmem.get(*cursor+1)? as usize;
   let op2 = *progmem.get(*cursor+2)? as usize;
   let dst = *progmem.get(*cursor+3)? as usize;

   progmem[dst] = f(progmem[op1], progmem[op2]);
   *cursor += 4;
   Some(())
}

fn run_computor(program: &Vec<u32>) -> Option<Vec<u32>> {
   let mut mem = program.clone();
   let mut cursor = 0;
   loop {
      match mem[cursor] {
         1 => opcode(&mut mem, &mut cursor, |a, b| a+b)?,
         2 => opcode(&mut mem, &mut cursor, |a, b| a*b)?,
         99 => return Some(mem),
         _ => return None
      }
   }
}

fn part1(progmem: &Vec<u32>) {
   if let Some(mem) = run_computor(progmem) {
      println!("{}", mem[0]);
   } else {
      eprintln!("computor failed for some reason");
   }
}

fn part2(progmem: &Vec<u32>) {
   let mut asd = progmem.clone();
   for noun in 1..99 {
      asd[1] = noun;
      for verb in 1..99 {
         asd[2] = verb;
         if let Some(mem) = run_computor(&asd) {
            if mem[0] == 19690720 {
               println!("{}", 100 * noun + verb);
            }
         }
      }
   }
}

fn main() {
   let inp = fs::read_to_string("input").unwrap();
   let mut progmem: Vec<u32> = inp
      .trim()
      .split(",")
      .map(|s| s.parse().unwrap())
      .collect();

   progmem[1] = 12;
   progmem[2] = 2;

   part2(&progmem);

}
