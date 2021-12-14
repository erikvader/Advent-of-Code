#[derive(Debug,PartialEq,Eq,Copy,Clone)]
struct Point {
   x: i32,
   y: i32
}

#[derive(Eq,PartialEq,Debug)]
enum Dir {
   UP, DOWN, LEFT, RIGHT
}

#[derive(Debug)]
struct Line {
   p: Point,
   len: i32,
   d: Dir
}

impl Point {
   fn new(x: i32, y: i32) -> Point {
      Point{x: x, y: y}
   }
   fn manhattan(self, other: Self) -> i32 {
      (self.x - other.x).abs() + (self.y - other.y).abs()
   }
}

impl Dir {
   fn from_string(s: &str) -> Dir {
      match s {
         "R" => Dir::RIGHT,
         "L" => Dir::LEFT,
         "U" => Dir::UP,
         "D" => Dir::DOWN,
         _ => panic!("elkajsjdj")
      }
   }
}

impl Line {
   fn new(p: Point, len: i32, d: &str) -> Line {
      Self::new2(p, len, Dir::from_string(d))
   }
   fn new2(p: Point, len: i32, d: Dir) -> Line {
      Line {p: p, len: len, d: d}
   }
   fn intersect(&self, other: &Self) -> Option<Point> {
      fn between(l1: i32, l2: i32, p: i32) -> bool {
         p >= l1 && p <= l2
      }
      fn partial(this: &Line, with: &Line) -> bool {
         let Point {x: ex, y: ey} = this.end_point();
         if this.d == Dir::UP {
            between(this.p.y, ey, with.p.y)
         } else if this.d == Dir::DOWN {
            between(ey, this.p.y, with.p.y)
         } else if this.d == Dir::RIGHT {
            between(this.p.x, ex, with.p.x)
         } else {
            between(ex, this.p.x, with.p.x)
         }
      }
      if self.is_vertical() != other.is_vertical() && partial(self, other) && partial(other, self) {
         if self.is_vertical() {
            Some(Point::new(self.p.x, other.p.y))
         } else {
            Some(Point::new(other.p.x, self.p.y))
         }
      } else {
         None
      }
   }
   fn end_point(&self) -> Point {
      let Point {mut x, mut y} = self.p;
      match self.d {
         Dir::UP => y += self.len,
         Dir::DOWN => y -= self.len,
         Dir::LEFT => x -= self.len,
         Dir::RIGHT => x += self.len
      }
      Point::new(x, y)
   }
   fn is_vertical(&self) -> bool {
      self.d == Dir::UP || self.d == Dir::DOWN
   }
   fn on(&self, p: Point) -> Option<i32> {
      let tmp = Line::new2(p, 0, if self.is_vertical() {Dir::RIGHT} else {Dir::UP});
      if self.intersect(&tmp).is_some() {
         Some(p.manhattan(self.p))
      } else {
         None
      }
   }
}

fn trace(path: &str) -> Vec<Line> {
   let mut res = Vec::new();
   let mut cur = Point::new(0, 0);
   for ins in path.split(",") {
      let dir: String = ins.chars().take(1).collect();
      let len: i32 = ins.chars().skip(1).collect::<String>().parse().unwrap();
      let l = Line::new(cur, len, &dir);
      cur = l.end_point();
      res.push(l);
   }
   res
}

fn intersections(a: &Vec<Line>, b: &Vec<Line>) -> Vec<Point> {
   let mut res = Vec::new();
   for i in a {
      for j in b {
         if let Some(inter) = i.intersect(j) {
            res.push(inter);
         }
      }
   }
   res
}

fn steps(ls: &Vec<Line>, t: Point) -> i32 {
   let mut total = 0;
   for l in ls {
      let on = l.on(t);
      total += match on {
         Some(d) => d,
         None => l.len
      };
      if on.is_some() {
         break
      }
   }
   total
}

fn part1(inters: &Vec<Point>) {
   let min = inters.iter()
      .filter(|p| **p != Point::new(0, 0))
      .map(|p| p.manhattan(Point::new(0, 0)))
      .min().unwrap();

   println!("{}", min);
}

fn part2(inters: &Vec<Point>, segs1: &Vec<Line>, segs2: &Vec<Line>) {
   let min = inters
      .iter()
      .filter(|p| **p != Point::new(0, 0))
      .map(|i| steps(segs1, *i) + steps(segs2, *i))
      .min()
      .unwrap();

   println!("{}", min);
}

fn main() {
   let filecont = std::fs::read_to_string("input").unwrap();
   let lines: Vec<&str> = filecont.lines().collect();
   let path1 = lines[0];
   let path2 = lines[1];

   let segs1 = trace(path1);
   let segs2 = trace(path2);

   let inters: Vec<Point> = intersections(&segs1, &segs2);

   part1(&inters);
   part2(&inters, &segs1, &segs2);
}
