fn gcd(mut a: u64, mut b: u64) -> u64 {
   loop {
      if a == 0 {
         return b
      } else if b == 0 {
         return a
      } else if a > b {
         a = a - b;
      } else if a < b {
         b = b - a;
      } else {
         return a
      }
   }
}

fn lcm(a: u64, b: u64) -> u64 {
   (a / gcd(a, b)) * b
}

fn add(a: &mut [i32], b: &[i32]) {
   for (aa, bb) in a.iter_mut().zip(b) {
      *aa += bb;
   }
}

fn adjust_vel(pos: &[i32], vel: &mut [i32]) {
   for i in 0..vel.len() {
      let higher = pos.iter().filter(|p| **p > pos[i]).count() as i32;
      let lesser = pos.iter().filter(|p| **p < pos[i]).count() as i32;
      vel[i] += higher - lesser;
   }
}

fn step(pos: &mut [i32], vel: &mut [i32]) {
   adjust_vel(pos, vel);
   add(pos, vel);
}

fn loops_at(pos: &[i32], vel: &[i32]) -> u64 {
   let mut p = [0; 4];
   let mut v = [0; 4];
   p.copy_from_slice(pos);
   v.copy_from_slice(vel);
   let mut c = 0;
   loop {
      c += 1;
      step(&mut p, &mut v);
      if p == pos && v == vel {
         break
      }
   }
   c
}

fn main() {
   let xp = [-5, -8, 1, 11];
   let xv = [0; 4];
   let yp = [6, -4, 16, 11];
   let yv = [0; 4];
   let zp = [-11, -2, 4, -4];
   let zv = [0; 4];

   let xloop = loops_at(&xp, &xv);
   let yloop = loops_at(&yp, &yv);
   let zloop = loops_at(&zp, &zv);
   println!("{}, {}, {}", xloop, yloop, zloop);
   println!("{}", lcm(lcm(xloop, yloop), zloop));
}
