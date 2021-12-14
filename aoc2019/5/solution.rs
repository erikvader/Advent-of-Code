use std::fs;
use std::io::{self, BufRead, Write};

type Memory = Vec<i32>;

struct Computer {
   mem: Memory,
   cur: usize
}

#[derive(Debug)]
struct Instruction {
   opcode: u8,
   arg_modes: Vec<u8>
}

// TODO: bounds check everything. Now is only half of the stuff bounds
// checked. And they should use Result instead of Option.
impl Computer {
   pub fn new(mem: Memory) -> Self {
      Computer{mem: mem, cur: 0}
   }
   pub fn jmp(&mut self, addr: usize) {
      self.cur = addr;
   }
   pub fn step(&mut self, step: usize) {
      self.cur += step;
   }
   pub fn get(&self, addr: usize) -> i32 {
      self.mem[addr]
   }
   pub fn set(&mut self, addr: usize, data: i32) {
      self.mem[addr] = data
   }
   pub fn dereference(&self, addr: usize) -> i32 {
      self.get(self.get(addr) as usize)
   }
   pub fn pointer_set(&mut self, addr: usize, data: i32) {
      self.set(self.get(addr) as usize, data)
   }
   pub fn run(mut self) -> Option<Memory> {
      loop {
         let ins = Instruction::new(self.get(self.cur));
         match ins.apply(&mut self) {
            Some(true) => (),
            Some(false) => break,
            None => return None
         }
      }
      Some(self.mem)
   }
}

impl Instruction {
   pub fn new(code: i32) -> Self {
      let opcode = code % 100;
      let mut tmp = code / 100;
      let mut arg_modes = Vec::new();
      while tmp > 0 {
         arg_modes.push((tmp % 10) as u8);
         tmp /= 10;
      }
      Instruction {opcode: opcode as u8, arg_modes: arg_modes}
   }
   fn get_arg_mode(&self, i: usize) -> u8 {
      self.arg_modes.get(i).cloned().unwrap_or(0)
   }
   pub fn apply(&self, com: &mut Computer) -> Option<bool> {
      match self.opcode {
         1 => self.binary_result_pointer(com, |a, b| a+b),
         2 => self.binary_result_pointer(com, |a, b| a*b),
         3 => self.input(com),
         4 => self.output(com),
         5 => self.unary_test_jmp(com, |x| x != 0),
         6 => self.unary_test_jmp(com, |x| x == 0),
         7 => self.binary_result_pointer(com, |a, b| if a < b { 1 } else { 0 }),
         8 => self.binary_result_pointer(com, |a, b| if a == b { 1 } else { 0 }),
         99 => return Some(false),
         _ => return None
      }
      Some(true)
   }
   fn get_operand(&self, i: usize, com: &Computer) -> i32 {
      match self.get_arg_mode(i) {
         0 => com.dereference(com.cur + i + 1),
         1 => com.get(com.cur + i + 1),
         _ => panic!("lkasjdlk")
      }
   }
   fn binary_result_pointer<F>(&self, com: &mut Computer, f: F)
      where F: Fn(i32, i32) -> i32
   {
      let op1 = self.get_operand(0, com);
      let op2 = self.get_operand(1, com);
      com.pointer_set(com.cur + 3, f(op1, op2));
      com.step(4);
   }
   fn unary_test_jmp<F>(&self, com: &mut Computer, f: F)
      where F: Fn(i32) -> bool
   {
      let op1 = self.get_operand(0, com);
      let op2 = self.get_operand(1, com);
      if f(op1) {
         com.jmp(op2 as usize);
      } else {
         com.step(3);
      }
   }
   fn output(&self, com: &mut Computer) {
      println!("{}", self.get_operand(0, com));
      com.step(2);
   }
   fn input(&self, com: &mut Computer) {
      print!("> ");
      io::stdout().lock().flush().unwrap();
      let asd: i32 = io::stdin().lock().lines().next().unwrap().unwrap().parse().unwrap();
      com.pointer_set(com.cur + 1, asd);
      com.step(2);
   }
}

fn main() {
   let inp = fs::read_to_string("input").unwrap();
   let progmem: Memory = inp
      .trim()
      .split(",")
      .map(|s| s.parse().unwrap())
      .collect();

   let computer = Computer::new(progmem);
   computer.run();

}
