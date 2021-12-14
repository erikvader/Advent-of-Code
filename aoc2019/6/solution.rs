use std::fs::File;
use std::io::{BufReader, BufRead};
use std::collections::{HashMap, VecDeque};

fn read_input() -> Vec<(String, String)> {
   let f = File::open("input").unwrap();
   let reader = BufReader::new(f);
   let mut orbits: Vec<(String, String)> = Vec::new();
   for l in reader.lines() {
      let ll = l.unwrap();
      let pb: Vec<&str> = ll.split(')').collect();
      orbits.push((pb[0].to_string(), pb[1].to_string()));
   }
   orbits
}

fn path<'a>(parents: &'a HashMap<String, String>, start: &'a str) -> Vec<&'a str> {
   let mut path = vec![start];
   while let Some(p) = parents.get(*path.last().unwrap()) {
      path.push(p);
   }
   path
}

fn main() {
   let mut queue: VecDeque<(String, String)> = read_input().into_iter().collect();
   let mut pending: HashMap<String, Vec<String>> = HashMap::new();
   let mut dists: HashMap<String, u32> = HashMap::new();
   let mut parents: HashMap<String, String> = HashMap::new();

   dists.insert("COM".to_string(), 0);

   while !queue.is_empty() {
      let (p, b) = queue.pop_front().unwrap();
      if dists.contains_key(&p) {
         if pending.contains_key(&b) {
            for y in pending.remove(&b).unwrap().into_iter() {
               queue.push_front((b.clone(), y));
            }
         }
         dists.insert(b.clone(), dists.get(&p).unwrap() + 1);
         parents.insert(b, p);
      } else {
         pending.entry(p)
            .or_insert_with(Vec::new)
            .push(b);
      }
   }

   let part1: u32 = dists.values().sum();
   println!("part1: {}", part1);

   let mut path1 = path(&parents, "YOU");
   let mut path2 = path(&parents, "SAN");

   while path1.last().and_then(|a| path2.last().map(|b| a == b)).unwrap_or(false) {
      path1.pop();
      path2.pop();
   }

   let part2 = path1.len() - 1 + path2.len() - 1;
   println!("part2: {}", part2);
}
