use std::collections::HashMap;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct Ingredient(u64, String);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct Production {
   target: Ingredient,
   ingredients: Vec<Ingredient>
}

impl FromStr for Ingredient {
   type Err = ();
   fn from_str(s: &str) -> Result<Self, Self::Err> {
      let mut landr = s.trim().split(" ");
      let count = landr.next().ok_or(())?.parse().or(Err(()))?;
      let name = landr.next().ok_or(())?.to_string();
      Ok(Ingredient(count, name))
   }
}

impl FromStr for Production {
   type Err = ();
   fn from_str(s: &str) -> Result<Self, Self::Err> {
      let landr: Vec<&str> = s.split("=>").take(2).collect();
      let r: Ingredient = landr.get(1).ok_or(())?.parse()?;
      let l: Result<Vec<Ingredient>, _> = landr.get(0).ok_or(())?
         .split(",")
         .map(|p| p.parse::<Ingredient>())
         .collect();
      Ok(Production{target: r, ingredients: l?})
   }
}

fn build<'a>(
   building: &'a str,
   how_many: u64,
   stash: &mut HashMap<&'a str, u64>,
   productions: &'a HashMap<String, Production>
) -> u64
{
   if building == "ORE" {
      return how_many
   }

   let p = productions.get(building).unwrap();
   let i_already_have = stash.get(building).copied().unwrap_or(0);
   if i_already_have >= how_many {
      return 0;
   }

   let need_to_build = how_many - i_already_have;
   let whole_prod = (need_to_build / p.target.0) + (if need_to_build % p.target.0 == 0 {0} else {1});
   let mut ores = 0;
   for d in p.ingredients.iter() {
      ores += build(d.1.as_str(), d.0 * whole_prod, stash, productions);
      stash.entry(d.1.as_str()).and_modify(|x| *x -= d.0 * whole_prod);
   }
   *stash.entry(building)
      .or_insert(0) += p.target.0 * whole_prod;

   return ores
}

fn main() {
   let s = std::fs::read_to_string("input").unwrap();
   let mut productions = HashMap::new();
   for l in s.lines() {
      let prod = l.parse::<Production>().unwrap();
      productions.insert(prod.target.1.clone(), prod);
   }

   // part 1
   let mut stash = HashMap::new();
   let ore = build("FUEL", 1, &mut stash, &productions);
   println!("1 FUEL needs {} ore", ore);

   // part 2
   let target: u64 = 1000000000000;
   let mut m = 1;
   let mut n = target;
   while m <= n {
      let mid = (m + n) / 2;
      stash.clear();
      let ore = build("FUEL", mid, &mut stash, &productions);
      if ore <= target {
         m = mid + 1;
         if ore == target {
            break
         }
      } else {
         n = mid - 1;
      }
   }

   println!("{}", m - 1);
}
