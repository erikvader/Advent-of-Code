use std::fs;
use std::collections::{HashSet, VecDeque};
use std::iter::repeat;

type Memory = Vec<i32>;
type ComputerResult<T> = Result<T, ComputerError>;

#[derive(Debug)]
enum ComputerError {
   InvalidOpcode,
   OutOfInput,
   Segfault,
   IsHalted
}

#[derive(Clone)]
struct Computer {
   mem: Memory,
   cur: usize,
   input: VecDeque<i32>,
   output: VecDeque<i32>,
   is_halted: bool
}

#[derive(Debug)]
struct Instruction {
   opcode: u8,
   arg_modes: Vec<u8>
}

impl Computer {
   pub fn new(mem: Memory) -> Self {
      Computer{mem: mem, cur: 0, input: VecDeque::new(), output: VecDeque::new(), is_halted: false}
   }
   pub fn provide_single_input(&mut self, i: i32) {
      self.input.push_back(i)
   }
   pub fn provide_input(&mut self, input: &Vec<i32>) {
      for e in input.iter() {
         self.provide_single_input(*e)
      }
   }
   fn jmp(&mut self, addr: usize) -> ComputerResult<()> {
      if addr >= self.mem.len() {
         return Err(ComputerError::Segfault)
      }
      Ok(self.cur = addr)
   }
   fn step(&mut self, step: usize) -> ComputerResult<()> {
      if self.cur >= self.mem.len() - 1 {
         return Err(ComputerError::Segfault)
      }
      Ok(self.cur += step)
   }
   fn get(&self, addr: usize) -> ComputerResult<i32> {
      self.mem.get(addr).copied().ok_or(ComputerError::Segfault)
   }
   fn set(&mut self, addr: usize, data: i32) -> ComputerResult<()> {
      if let Some(d) = self.mem.get_mut(addr) {
         *d = data;
         Ok(())
      } else {
         Err(ComputerError::Segfault)
      }
   }
   fn dereference(&self, addr: usize) -> ComputerResult<i32> {
      self.get(addr).and_then(|x| self.get(x as usize))
   }
   fn pointer_set(&mut self, addr: usize, data: i32) -> ComputerResult<()> {
      self.get(addr).and_then(|x| self.set(x as usize, data))
   }
   pub fn run(&mut self) -> ComputerResult<()> {
      if self.is_halted() {
         return Err(ComputerError::IsHalted)
      }
      loop {
         let ins = Instruction::new(self.get(self.cur)?);
         match ins.apply(self) {
            Ok(true) => (),
            Ok(false) => {
               self.is_halted = true;
               return Ok(())
            },
            Err(e) => return Err(e)
         }
      }
   }
   fn read(&mut self) -> ComputerResult<i32> {
      self.input.pop_front().ok_or(ComputerError::OutOfInput)
   }
   fn write(&mut self, b: i32) {
      self.output.push_back(b);
   }
   fn is_halted(&self) -> bool {
      self.is_halted
   }
   fn get_output<'a>(&'a mut self) -> &'a mut VecDeque<i32> {
      return &mut self.output
   }
}

impl Instruction {
   fn new(code: i32) -> Self {
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
   fn apply(&self, com: &mut Computer) -> ComputerResult<bool> {
      match self.opcode {
         1 => self.binary_result_pointer(com, |a, b| a+b)?,
         2 => self.binary_result_pointer(com, |a, b| a*b)?,
         3 => self.input(com)?,
         4 => self.output(com)?,
         5 => self.unary_test_jmp(com, |x| x != 0)?,
         6 => self.unary_test_jmp(com, |x| x == 0)?,
         7 => self.binary_result_pointer(com, |a, b| if a < b { 1 } else { 0 })?,
         8 => self.binary_result_pointer(com, |a, b| if a == b { 1 } else { 0 })?,
         99 => return Ok(false),
         _ => return Err(ComputerError::InvalidOpcode)
      }
      Ok(true)
   }
   fn get_operand(&self, i: usize, com: &Computer) -> ComputerResult<i32> {
      match self.get_arg_mode(i) {
         0 => com.dereference(com.cur + i + 1),
         1 => com.get(com.cur + i + 1),
         _ => Err(ComputerError::InvalidOpcode)
      }
   }
   fn binary_result_pointer<F>(&self, com: &mut Computer, f: F) -> ComputerResult<()>
      where F: Fn(i32, i32) -> i32
   {
      let op1 = self.get_operand(0, com)?;
      let op2 = self.get_operand(1, com)?;
      com.pointer_set(com.cur + 3, f(op1, op2))?;
      com.step(4)?;
      Ok(())
   }
   fn unary_test_jmp<F>(&self, com: &mut Computer, f: F) -> ComputerResult<()>
      where F: Fn(i32) -> bool
   {
      let op1 = self.get_operand(0, com)?;
      let op2 = self.get_operand(1, com)?;
      if f(op1) {
         com.jmp(op2 as usize)?;
      } else {
         com.step(3)?;
      }
      Ok(())
   }
   fn output(&self, com: &mut Computer) -> ComputerResult<()> {
      com.write(self.get_operand(0, com)?);
      com.step(2)?;
      Ok(())
   }
   fn input(&self, com: &mut Computer) -> ComputerResult<()> {
      let asd = com.read()?;
      com.pointer_set(com.cur + 1, asd)?;
      com.step(2)?;
      Ok(())
   }
}

fn part1(progmem: &Memory, thruster: u8, input: i32, used: &mut HashSet<i32>) -> i32 {
   if thruster == 0 {
      return input;
   }
   let mut max = [0; 5];
   for i in 0..=4 {
      if used.contains(&i){
         continue;
      }
      used.insert(i);
      let input = vec![i, input];
      let mut c = Computer::new(progmem.clone());
      c.provide_input(&input);
      c.run().unwrap();
      max[i as usize] = part1(progmem, thruster - 1, c.get_output().pop_front().unwrap(), used);
      used.remove(&i);
   }
   *max.into_iter().max().unwrap()
}

fn run_machines(progmem: &Memory, settings: Vec<i32>) -> i32 {
   let mut amps: Vec<Computer> = repeat(Computer::new(progmem.clone())).take(5).collect();
   for (amp, setting) in amps.iter_mut().zip(settings.iter()) {
      amp.provide_single_input(*setting);
   }
   let mut prev_input = 0;
   let mut i = 0;
   loop {
      amps[i].provide_single_input(prev_input);
      match amps[i].run() {
         Ok(()) if i == 4 => return amps[i].get_output().pop_front().unwrap(),
         Ok(()) => (),
         Err(ComputerError::OutOfInput) => (),
         Err(e) => Err(e).unwrap()
      }
      prev_input = amps[i].get_output().pop_front().unwrap();
      i += 1;
      i %= 5;
   }
}

fn perms(work: &mut Vec<i32>, l: usize, res: &mut Vec<Vec<i32>>) {
   if l >= work.len() - 1 {
      res.push(work.clone());
   } else {
      for i in l..work.len() {
         work.swap(l, i);
         perms(work, l+1, res);
         work.swap(l, i);
      }
   }
}

fn part2(progmem: &Memory) -> i32 {
   let mut settings = Vec::new();
   perms(&mut vec![5,6,7,8,9], 0, &mut settings);
   let max = settings.into_iter()
      .map(|p| run_machines(progmem, p))
      .max()
      .unwrap();
   max
}

fn main() {
   let inp = fs::read_to_string("input").unwrap();
   let progmem: Memory = inp
      .trim()
      .split(",")
      .map(|s| s.trim().parse().unwrap())
      .collect();

   // let part1 = part1(&progmem, 5, 0, &mut HashSet::new());
   // println!("{}", part1);

   let part2 = part2(&progmem);
   println!("{}", part2);
}
