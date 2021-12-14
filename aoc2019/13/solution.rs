mod intcode;

use intcode::Computer;
use std::collections::HashMap;
use std::cmp::Ordering::*;

fn part1(com: &mut Computer) {
   com.run().unwrap();

   let mut screen: HashMap<(i64, i64), i64> = HashMap::new();
   let mut buf = [0; 3];
   while com.pop_next_outputs(&mut buf) {
      screen.insert((buf[0], buf[1]), buf[2]);
   }

   let num_block_tiles = screen.values().filter(|v| **v == 2).count();
   println!("{}", num_block_tiles);
}

fn part2(com: &mut Computer) {
   com.magnet_poke(0, 2).expect("couldn't set number of quarters");

   let mut score = 0;
   let mut ballx = 0;
   let mut paddlex = 0;
   let mut lekpinne = 0;

   while !com.is_halted() {
      com.provide_single_input(lekpinne);
      com.run_partial().unwrap();
      let mut buf = [0; 3];
      while com.pop_next_outputs(&mut buf) {
         let [x, y, t] = buf;
         if x == -1 && y == 0 {
            score = t;
         } else if t == 4 {
            ballx = x;
         } else if t == 3 {
            paddlex = x;
         }
      }

      lekpinne = match paddlex.cmp(&ballx) {
         Equal => 0,
         Less => 1,
         Greater => -1
      }
   }

   println!("{}", score);
}

fn main() {
   let s = std::fs::read_to_string("input").unwrap();
   let mut com = Computer::from_string(&s);
   // part1(&mut com);
   part2(&mut com);
}
