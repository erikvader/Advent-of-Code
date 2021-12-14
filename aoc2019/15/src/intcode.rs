#![allow(dead_code)]
use std::collections::VecDeque;

pub type Data = i64;
type Memory = Vec<Data>;
pub type ComputerResult<T> = Result<T, ComputerError>;

#[derive(Debug)]
pub enum ComputerError {
   InvalidOpcode,
   OutOfInput,
   Segfault,
   IsHalted
}

#[derive(Clone)]
pub struct Computer {
   mem: Memory,
   cur: usize,
   base: usize,
   input: VecDeque<Data>,
   output: VecDeque<Data>,
   is_halted: bool
}

#[derive(Debug)]
struct Instruction {
   opcode: u8,
   arg_modes: Vec<u8>
}

impl Computer {
   pub fn new(mem: Memory) -> Self {
      Computer{mem: mem,
               cur: 0,
               base: 0,
               input: VecDeque::new(),
               output: VecDeque::new(),
               is_halted: false}
   }
   pub fn from_string(inp: &str) -> Self {
      let progmem: Memory = inp
         .trim()
         .split(",")
         .map(|s| s.trim().parse().unwrap())
         .collect();
      Self::new(progmem)
   }
   pub fn magnet_poke(&mut self, addr: usize, data: Data) -> ComputerResult<()> {
      self.set(addr, data)
   }
   pub fn provide_single_input(&mut self, i: Data) {
      self.input.push_back(i)
   }
   pub fn provide_input(&mut self, input: &Vec<Data>) {
      for e in input.iter() {
         self.provide_single_input(*e)
      }
   }
   fn cursor(&self) -> usize {
      self.cur
   }
   fn base(&self) -> usize {
      self.base
   }
   fn adjust_base(&mut self, offset: Data) {
      self.base = (self.base() as Data + offset) as usize
   }
   fn brk(&mut self, addr: usize) {
      if addr >= self.mem.len() {
         self.mem.resize(addr+1, 0);
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
   fn get(&self, addr: usize) -> ComputerResult<Data> {
      if addr >= self.mem.len() {
         return Ok(0)
      }
      self.mem.get(addr).copied().ok_or(ComputerError::Segfault)
   }
   fn set(&mut self, addr: usize, data: Data) -> ComputerResult<()> {
      self.brk(addr);
      if let Some(d) = self.mem.get_mut(addr) {
         *d = data;
         Ok(())
      } else {
         Err(ComputerError::Segfault)
      }
   }
   fn dereference(&self, addr: usize) -> ComputerResult<Data> {
      self.get(addr).and_then(|x| self.get(x as usize))
   }
   fn pointer_set(&mut self, addr: usize, data: Data) -> ComputerResult<()> {
      self.get(addr).and_then(|x| self.set(x as usize, data))
   }
   fn get_base(&self, addr: usize) -> ComputerResult<Data> {
      self.get((self.base as Data + self.get(addr)?) as usize)
   }
   fn set_base(&mut self, addr: usize, data: Data) -> ComputerResult<()> {
      self.set((self.base as Data + self.get(addr)?) as usize, data)
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
   pub fn run_partial(&mut self) -> ComputerResult<()> {
      match self.run() {
         Err(ComputerError::OutOfInput) => Ok(()),
         x => x
      }
   }
   fn read(&mut self) -> ComputerResult<Data> {
      self.input.pop_front().ok_or(ComputerError::OutOfInput)
   }
   fn write(&mut self, b: Data) {
      self.output.push_back(b);
   }
   pub fn is_halted(&self) -> bool {
      self.is_halted
   }
   pub fn get_output<'a>(&'a mut self) -> &'a mut VecDeque<Data> {
      return &mut self.output
   }
   pub fn pop_next_outputs(&mut self, buf: &mut [Data]) -> bool {
      for i in 0..buf.len() {
         if let Some(n) = self.output.pop_front() {
            buf[i] = n;
         } else {
            return false
         }
      }
      return true
   }
   pub fn dump_to_stdout(&mut self) {
      while !self.output.is_empty() {
         println!("{}", self.output.pop_front().unwrap());
      }
   }
}

impl Instruction {
   fn new(code: Data) -> Self {
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
         9 => self.adjust_base(com)?,
         99 => return Ok(false),
         _ => return Err(ComputerError::InvalidOpcode)
      }
      Ok(true)
   }
   fn get_operand(&self, i: usize, com: &Computer) -> ComputerResult<Data> {
      match self.get_arg_mode(i) {
         0 => com.dereference(com.cursor() + i + 1),
         1 => com.get(com.cursor() + i + 1),
         2 => com.get_base(com.cursor() + i + 1),
         _ => Err(ComputerError::InvalidOpcode)
      }
   }
   fn write_to(&self, com: &mut Computer, i: usize, data: Data) -> ComputerResult<()> {
      match self.get_arg_mode(i) {
         0 | 1 => com.pointer_set(com.cursor() + i + 1, data),
         2     => com.set_base(com.cursor() + i + 1, data),
         _ => Err(ComputerError::InvalidOpcode)
      }
   }
   fn binary_result_pointer<F>(&self, com: &mut Computer, f: F) -> ComputerResult<()>
      where F: Fn(Data, Data) -> Data
   {
      let op1 = self.get_operand(0, com)?;
      let op2 = self.get_operand(1, com)?;
      self.write_to(com, 2, f(op1, op2))?;
      com.step(4)?;
      Ok(())
   }
   fn adjust_base(&self, com: &mut Computer) -> ComputerResult<()> {
      let op1 = self.get_operand(0, com)?;
      com.adjust_base(op1);
      com.step(2)?;
      Ok(())
   }
   fn unary_test_jmp<F>(&self, com: &mut Computer, f: F) -> ComputerResult<()>
      where F: Fn(Data) -> bool
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
      self.write_to(com, 0, asd)?;
      com.step(2)?;
      Ok(())
   }
}
