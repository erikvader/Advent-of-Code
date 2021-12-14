mod intcode;

use intcode::Computer;
use std::collections::{HashSet, VecDeque, HashMap};

#[derive(Debug,PartialEq,Eq,Copy,Clone,Hash)]
struct Point {
   x: i32,
   y: i32
}

impl Point {
   fn new(x: i32, y: i32) -> Point {
      Point{x: x, y: y}
   }
   fn offset(self, dx: i32, dy: i32) -> Self {
      Point::new(self.x + dx, self.y + dy)
   }
   fn walk(self, dir: u8) -> Self {
      match dir {
         1 => self.offset(0, 1),
         2 => self.offset(0, -1),
         3 => self.offset(-1, 0),
         4 => self.offset(1, 0),
         _ => panic!("asdasd")
      }
   }
}

fn part1(c: &Computer) -> Computer {
   let mut queue: VecDeque<_> = VecDeque::new();
   let mut visited = HashSet::new();
   let mut parents = HashMap::new();

   queue.push_back((Point::new(0, 0), c.clone()));
   visited.insert(Point::new(0, 0));

   let mut oxygen_system = None;
   'outer:
   while !queue.is_empty() {
      let (poi, com) = queue.pop_front().unwrap();

      for dir in 1..=4 {
         let next_poi = poi.walk(dir as u8);
         if !visited.insert(next_poi) {
            continue
         }
         let mut new_com = com.clone(); // TODO: optimize this
         new_com.provide_single_input(dir);
         new_com.run_partial().unwrap();
         let status = new_com.get_output().pop_front().unwrap();
         if status == 0 {
            continue
         }
         parents.insert(next_poi, poi);
         if status == 1 {
            queue.push_back((next_poi, new_com));
         } else if status == 2 {
            oxygen_system = Some((next_poi, new_com));
            break 'outer;
         }
      }
   }

   if let Some((oxy, com)) = oxygen_system {
      let mut count = 0;
      let mut cur = oxy;
      while let Some(p) = parents.get(&cur) {
         count += 1;
         cur = *p;
      }
      println!("{}", count);
      return com;
   }
   unreachable!()
}

fn part2(c: &Computer) {
   let mut queue: VecDeque<_> = VecDeque::new();
   let mut visited = HashSet::new();

   queue.push_back(Some((Point::new(0, 0), c.clone())));
   queue.push_back(None);
   visited.insert(Point::new(0, 0));

   let mut layers = 0;

   'outer:
   while !queue.is_empty() {
      let next = queue.pop_front().unwrap();
      if next.is_none() {
         if !queue.is_empty() {
            queue.push_back(None);
            layers += 1;
         }
         continue
      }
      let (poi, com) = next.unwrap();

      for dir in 1..=4 {
         let next_poi = poi.walk(dir as u8);
         if !visited.insert(next_poi) {
            continue
         }
         let mut new_com = com.clone(); // TODO: optimize this
         new_com.provide_single_input(dir);
         new_com.run_partial().unwrap();
         let status = new_com.get_output().pop_front().unwrap();
         if status == 0 {
            continue
         } else {
            queue.push_back(Some((next_poi, new_com)));
         }
      }
   }
   println!("{}", layers);
}

fn main() {
   let c = Computer::from_string(&std::fs::read_to_string("input").unwrap());
   let oxygen_system = part1(&c);
   part2(&oxygen_system);
}
