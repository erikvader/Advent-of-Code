struct PatternIter<'a> {
   pattern: &'a [i32],
   repeat: usize,
   i: usize,
   j: usize
}

impl<'a> PatternIter<'a> {
   fn new(pattern: &'a [i32], repeat: usize) -> Self {
      assert!(repeat > 0);
      PatternIter{pattern: pattern, repeat: repeat, i: 0, j: 0}
   }
}

impl<'a> Iterator for PatternIter<'a> {
   type Item = i32;

   fn next(&mut self) -> Option<Self::Item> {
      let to_output = self.pattern[self.i];
      self.j += 1;
      if self.j >= self.repeat {
         self.j = 0;
         self.i += 1;
         self.i %= self.pattern.len();
      }
      Some(to_output)
   }
}

fn to_digits(s: &str) -> Vec<i32> {
   let mut v = Vec::new();
   for d in s.chars() {
      if !d.is_digit(10) {
         continue;
      }
      v.push(d.to_digit(10).unwrap() as i32);
   }
   v
}

fn from_digits(ds: &[i32]) -> i32 {
   let mut x = 0;
   for d in ds.iter().rev() {
      x *= 10;
      x += d;
   }
   x
}

fn phase(input: &[i32], pattern: &[i32], output: &mut [i32]) {
   for i in 0..output.len() {
      let pi = PatternIter::new(&pattern, i+1);
      output[i] = input.iter().zip(pi.skip(1))
         .map(|(a, b)| a*b)
         .sum();

      output[i] = output[i].abs() % 10;
   }
}

fn main() {
   let s = std::fs::read_to_string("input").unwrap();
   let mut input = to_digits(&s);
   let mut output = Vec::new();
   output.resize(input.len(), 0);
   let pattern = [0, 1, 0, -1];
   for _ in 0..100 {
      phase(&input, &pattern, &mut output);
      let tmp = input;
      input = output;
      output = tmp;
   }

   println!("{:?}", input);

}
