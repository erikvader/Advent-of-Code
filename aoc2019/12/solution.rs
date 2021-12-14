use std::ops::Add;
use std::cmp::Ordering;
use std::iter::FromIterator;
use std::cmp::max;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct Vek {
   x: i32,
   y: i32,
   z: i32
}

#[derive(Debug)]
struct Moon {
   pos: Vek,
   vel: Vek
}

impl Vek {
   fn new(x: i32, y: i32, z: i32) -> Self {
      Vek{x: x, y: y, z: z}
   }
   fn iter(&self) -> impl Iterator<Item = i32> {
      vec![self.x, self.y, self.z].into_iter() //this probably allocates everything on the heap
   }
}

impl Add for Vek {
   type Output = Vek;

   fn add(self, other: Self) -> Self {
      Vek{
         x: self.x + other.x,
         y: self.y + other.y,
         z: self.z + other.z
      }
   }
}

impl FromIterator<i32> for Vek {
   fn from_iter<I: IntoIterator<Item = i32>>(iter: I) -> Self {
      let mut i = iter.into_iter();
      Vek::new(
         i.next().unwrap(),
         i.next().unwrap(),
         i.next().unwrap()
      )
   }
}

impl Moon {
   fn new(x: i32, y: i32, z: i32) -> Self {
      Moon{vel: Vek::new(0, 0, 0),
           pos: Vek::new(x, y, z)}
   }
   fn gravity(&mut self, other: &Self) {
      let grav: Vek = self.pos.iter()
         .zip(other.pos.iter())
         .map(|(s, o)| match s.cmp(&o) {
            Ordering::Equal => 0,
            Ordering::Less => 1,
            Ordering::Greater => -1
         })
         .collect();

      self.vel = self.vel + grav;
   }
   fn step(&mut self) {
      self.pos = self.pos + self.vel;
   }
   fn energy(&self) -> i32 {
      let potential: i32 = self.pos.iter().map(|x| x.abs()).sum();
      let kinetic: i32 = self.vel.iter().map(|x| x.abs()).sum();
      potential * kinetic
   }
}

fn main() {
   let mut moons: Vec<Moon> = vec![
      Moon::new(-5, 6, -11),
      Moon::new(-8, -4, -2),
      Moon::new(1, 16, 4),
      Moon::new(11, 11, -4)
   ];
   // let mut moons: Vec<Moon> = vec![
   //    Moon::new(-1, 0, 2),
   //    Moon::new(2, -10, -7),
   //    Moon::new(4, -8, 8),
   //    Moon::new(3, 5, -1)
   // ];

   for _ in 0..1000 {
      for i in 0..moons.len() {
         for j in 0..moons.len() {
            if j == i {
               continue
            }
            let (l, r) = moons.split_at_mut(max(i, j));
            let (mi, mj) = if i < j {
               (&mut l[i], &mut r[0])
            } else {
               (&mut r[0], &mut l[j])
            };
            mi.gravity(mj);
         }
      }
      for m in moons.iter_mut() {
         m.step();
      }
   }

   // println!("{:?}", moons);

   println!("{}", moons.iter().map(|m| m.energy()).sum::<i32>());
}
