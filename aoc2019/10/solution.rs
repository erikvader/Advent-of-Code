use std::collections::{HashSet, VecDeque};
use std::iter::FromIterator;
use std::ops::{Add, Sub, Index};
use std::cmp::Ordering;
use std::fmt;

#[derive(PartialEq, Eq, Hash)]
struct Asteroids {
   rows: Vec<Vec<bool>>
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Point<'a> {
   x: usize,
   y: usize,
   ast: &'a Asteroids
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Change {
   dx: isize,
   dy: isize
}

impl Asteroids {
   pub fn is_outside(&self, p: &Point) -> bool {
      p.y >= self.rows.len()
         || p.x >= self.rows[p.y].len()
   }
   pub fn iter(&self) -> impl Iterator<Item = &Vec<bool>> + '_ {
      self.rows.iter()
   }
   pub fn collect_points<'a, T: FromIterator<Point<'a>>>(&'a self) -> T {
      self.iter()
         .enumerate()
         .flat_map(|(ri, r)| r.iter()
                   .enumerate()
                   .filter_map(move |(ci, c)| if *c {
                      Some(Point{x: ci, y: ri, ast: self})
                   } else {
                      None
                   }))
         .collect()
   }
}

impl fmt::Debug for Point<'_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "({}, {})", self.x, self.y)
   }
}

impl Index<Point<'_>> for Asteroids {
   type Output = bool;
   fn index(&self, index: Point) -> &Self::Output {
      &self.rows[index.y][index.x]
   }
}

impl<'a> Point<'a> {
   pub fn new(x: usize, y: usize, ast: &'a Asteroids) -> Option<Self> {
      let p = Point{x: x, y: y, ast: ast};
      if ast.is_outside(&p) {
         None
      } else {
         Some(p)
      }
   }
   pub fn angle_to(&self, other: &Self) -> f64 {
      let mut tmp = (other.y as f64 - self.y as f64).atan2(other.x as f64 - self.x as f64);
      tmp += std::f64::consts::FRAC_PI_2;
      if tmp < 0.0 {
         tmp += 2.0*std::f64::consts::PI;
      }
      tmp
   }
   pub fn compare_angle(&self, p1: &Self, p2: &Self) -> Ordering {
      let a1 = (self.angle_to(p1) * 10000.0).trunc() as i64;
      let a2 = (self.angle_to(p2) * 10000.0).trunc() as i64;
      a1.cmp(&a2)
   }
   pub fn compare(&self, p1: &Self, p2: &Self) -> Ordering {
      let c = self.compare_angle(p1, p2);
      if c == Ordering::Equal {
         self.manhattan(p1).cmp(&self.manhattan(p2))
      } else {
         c
      }
   }
   pub fn manhattan(&self, other: &Self) -> u64 {
      ((self.x as i64 - other.x as i64).abs() + (self.y as i64 - other.y as i64).abs()) as u64
   }
}

impl Change {
   pub fn normalize(self) -> Self {
      if self.dx == 0 && self.dy == 0 {
         return self
      }
      let g = gcd(self.dx.abs() as usize, self.dy.abs() as usize) as isize;
      Change{dx: self.dx / g, dy: self.dy / g}
   }
}

impl FromIterator<Vec<bool>> for Asteroids {
   fn from_iter<I: IntoIterator<Item=Vec<bool>>>(i: I) -> Self {
      Asteroids{rows: Vec::from_iter(i)}
   }
}

impl Add<Change> for Point<'_> {
   type Output = Option<Self>;
   fn add(self, other: Change) -> Option<Self> {
      let newx = self.x as isize + other.dx;
      let newy = self.y as isize + other.dy;
      if newx < 0 || newy < 0 {
         return None
      }
      Point::new(newx as usize, newy as usize, self.ast)
   }
}

impl Sub for Point<'_> {
   type Output = Change;
   fn sub(self, other: Self) -> Change {
      Change{dx: self.x as isize - other.x as isize,
             dy: self.y as isize - other.y as isize}
   }
}

fn gcd(a: usize, b: usize) -> usize {
   if a == 0 {
      b
   } else if b == 0 {
      a
   } else if a > b {
      gcd(a - b, b)
   } else if a < b {
      gcd(a, b - a)
   } else {
      a
   }
}

fn observable_asteroids<'a>(ast: &Asteroids, p: Point<'a>, mut queue: HashSet<Point<'a>>) -> u32 {
   let mut seen = 0;
   queue.remove(&p);
   while !queue.is_empty() {
      seen += 1;
      let tmp = queue.iter().next().cloned().unwrap();
      let cur = queue.take(&tmp).unwrap();
      let ch = (cur - p).normalize();

      let mut pos = p;
      while let Some(newp) = pos + ch {
         if ast[newp] {
            queue.remove(&newp);
         }
         pos = newp;
      }
   }
   seen
}

fn part1(grid: &Asteroids) {
   let queue: HashSet<Point> = grid.collect_points();

   let mut max = None;
   let mut max_cord = (0, 0);
   for (ri, r) in grid.iter().enumerate() {
      for (ci, c) in r.iter().enumerate() {
         if !c {
            continue
         }
         let m = observable_asteroids(&grid,
                                      Point::new(ci, ri, &grid).unwrap(),
                                      queue.clone());
         max = max.map(|n| std::cmp::max(m, n)).or(Some(m));
         if Some(m) == max {
            max_cord = (ci, ri);
         }
      }
   }
   println!("pos: {:?}, asteroids: {}", max_cord, max.unwrap());
}

fn part1_2(grid: &Asteroids) {
   let points: Vec<Point> = grid.collect_points();
   let mut found = HashSet::new();
   let mut max = None;
   for p1 in points.iter() {
      found.clear();
      for p2 in points.iter() {
         found.insert((*p2 - *p1).normalize());
      }
      let m = found.len() - 1;
      max = max.map(|n| std::cmp::max(m, n)).or(Some(m));
   }
   println!("{}", max.unwrap());
}

fn part2(grid: &Asteroids, base: Point) {
   let mut points: Vec<Point> = grid.collect_points();
   points.sort_unstable_by(|a, b| base.compare(a, b));

   let mut count = 0;
   let mut cur: VecDeque<_> = points.into_iter().collect();
   while !cur.is_empty() {
      count += 1;
      let x = cur.pop_front().unwrap();
      while !cur.is_empty() && base.compare_angle(&x, cur.front().unwrap()) == Ordering::Equal {
         cur.rotate_left(1);
      }
      if count == 200 {
         println!("{}", x.x*100 + x.y);
         break
      }

   }
}

fn main() {
   let inp = std::fs::read_to_string("input").expect("fil? hallå!?");
   let grid: Asteroids = inp
      .lines()
      .map(|l| l.chars().map(|c| c == '#').collect())
      .collect();

   part1_2(&grid); // pos (23, 29), asteroids 263
   part2(&grid, Point::new(23, 29, &grid).unwrap());
}
