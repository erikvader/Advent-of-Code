mod intcode;

use intcode::{Data, Computer};
use std::fs;
use std::collections::HashMap;
use std::cmp::Ordering;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Pos {
   x: isize,
   y: isize
}

struct Robot {
   dir: i8,
   pos: Pos
}

impl Pos {
   fn change(&mut self, dx: isize, dy: isize) {
      self.x += dx;
      self.y += dy;
   }
}

impl PartialOrd for Pos {
   fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
   }
}

impl Ord for Pos {
   fn cmp(&self, other: &Self) -> Ordering {
      other.y.cmp(&self.y).then(self.x.cmp(&other.x))
   }
}

impl Robot {
   fn step(&mut self) {
      match self.dir {
         0 => self.pos.change(0, 1),
         1 => self.pos.change(-1, 0),
         2 => self.pos.change(0, -1),
         3 => self.pos.change(1, 0),
         _ => panic!("asd")
      }
   }
   fn rotate(&mut self, dir: i8) {
      if dir == 0 {
         self.dir -= 1;
      } else {
         self.dir += 1;
      }
      self.dir += 4;
      self.dir %= 4;
   }
}

fn paint(grid: &HashMap<Pos, Data>) {
   // filter out black squares
   let mut white_squares: Vec<Pos> = grid.iter()
      .filter(|(_, v)| **v == 1)
      .map(|(k, _)| *k)
      .map(|Pos{x, y}| Pos{x: -x, y: y}) // it's backwards for some reason
      .collect();

   // sort grid by highest y then lowest x
   white_squares.sort_unstable();

   // find highest y and lowest x overall, this is the start position
   let maxy = white_squares.iter().map(|Pos{y, ..}| y).max().copied().unwrap();
   let minx = white_squares.iter().map(|Pos{x, ..}| x).min().copied().unwrap();

   let mut y = maxy;
   let mut x = minx;

   // loop these squares and try to print
   for p in white_squares.into_iter() {
      while p.y < y {
         println!("");
         y -= 1;
         x = minx;
      }
      print!("{}", " ".repeat((p.x - x).abs() as usize));
      print!("X");
      x = p.x + 1;
   }
   println!("");
}

fn main() {
   let inp = fs::read_to_string("input").unwrap();

   let mut com = Computer::from_string(&inp);
   let mut grid: HashMap<Pos, Data> = HashMap::new();
   let mut rob = Robot{dir: 0, pos: Pos{x: 0, y: 0}};

   // part 2
   grid.insert(Pos{x: 0, y: 0}, 1);

   while !com.is_halted() {
      let standing = grid.get(&rob.pos).copied().unwrap_or(0);
      com.provide_single_input(standing);
      com.run_partial().expect("computer run failed");

      let paint_to = com.get_output().pop_front().expect("didn't get a color to paint the square with");
      grid.insert(rob.pos, paint_to);

      let turn = com.get_output().pop_front().expect("didn't get a new turn");
      rob.rotate(turn as i8);
      rob.step();
   }

   println!("squares changed: {}", grid.len());
   paint(&grid);
}
