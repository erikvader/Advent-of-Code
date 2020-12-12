use std::convert::TryFrom;

#[derive(Clone, Copy, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[repr(u8)]
enum Car {
    N = 0,
    E,
    S,
    W,
}

struct Ship {
    x: i32,
    y: i32,
    dir: Car,
}

impl Car {
    fn right(self) -> Self {
        let mut n: u8 = self.into();
        n += 1;
        n %= 4;
        Self::try_from(n).unwrap()
    }

    fn left(self) -> Self {
        let mut n: u8 = self.into();
        n += 3;
        n %= 4;
        Self::try_from(n).unwrap()
    }

    fn riktning(self) -> (i32, i32) {
        match self {
            Self::E => (1, 0),
            Self::W => (-1, 0),
            Self::N => (0, 1),
            Self::S => (0, -1),
        }
    }
}

impl Ship {
    fn new() -> Self {
        Self {
            x: 0,
            y: 0,
            dir: Car::E,
        }
    }

    fn turn_right(&mut self, mut degrees: u32) {
        while degrees > 0 {
            degrees -= 90;
            self.dir = self.dir.right();
        }
    }

    fn turn_left(&mut self, mut degrees: u32) {
        while degrees > 0 {
            degrees -= 90;
            self.dir = self.dir.left();
        }
    }

    fn flytta(&mut self, steps: u32) {
        let (dx, dy) = self.dir.riktning();
        self.x += dx * steps as i32;
        self.y += dy * steps as i32;
    }

    fn instruction(&mut self, op: char, num: u32) {
        match op {
            'N' => self.y += num as i32,

            'S' => self.y -= num as i32,
            'E' => self.x += num as i32,
            'W' => self.x -= num as i32,
            'L' => self.turn_left(num),
            'R' => self.turn_right(num),
            'F' => self.flytta(num),
            _ => panic!("eeeto"),
        }
    }

    fn moved_distance(&self) -> u32 {
        (self.x.abs() + self.y.abs()) as u32
    }

    fn move_to(&mut self, wp: &Waypoint, times: u32) {
        self.x += wp.x * times as i32;
        self.y += wp.y * times as i32;
    }
}

struct Waypoint {
    x: i32,
    y: i32,
}

impl Waypoint {
    fn new() -> Self {
        Self { x: 10, y: 1 }
    }

    fn cw(&mut self) {
        std::mem::swap(&mut self.x, &mut self.y);
        self.y *= -1;
    }

    fn ccw(&mut self) {
        std::mem::swap(&mut self.x, &mut self.y);
        self.x *= -1;
    }

    fn turn_right(&mut self, mut degrees: u32) {
        while degrees > 0 {
            degrees -= 90;
            self.cw();
        }
    }

    fn turn_left(&mut self, mut degrees: u32) {
        while degrees > 0 {
            degrees -= 90;
            self.ccw();
        }
    }
}

struct WpShip {
    wp: Waypoint,
    ship: Ship,
}

impl WpShip {
    fn new() -> Self {
        Self {
            wp: Waypoint::new(),
            ship: Ship::new(),
        }
    }

    fn instruction(&mut self, op: char, num: u32) {
        match op {
            'N' => self.wp.y += num as i32,

            'S' => self.wp.y -= num as i32,
            'E' => self.wp.x += num as i32,
            'W' => self.wp.x -= num as i32,
            'L' => self.wp.turn_left(num),
            'R' => self.wp.turn_right(num),
            'F' => self.ship.move_to(&self.wp, num),
            _ => panic!("eeeto"),
        }
    }
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let moved_ship = input
        .lines()
        .map(|l| {
            let op = l.as_bytes()[0] as char;
            let num = l[1..].parse::<u32>().unwrap();
            (op, num)
        })
        .fold(Ship::new(), |mut ship, (op, num)| {
            ship.instruction(op, num);
            ship
        });

    let man = moved_ship.moved_distance();
    Ok(man.to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let moved_ship = input
        .lines()
        .map(|l| {
            let op = l.as_bytes()[0] as char;
            let num = l[1..].parse::<u32>().unwrap();
            (op, num)
        })
        .fold(WpShip::new(), |mut ship, (op, num)| {
            ship.instruction(op, num);
            ship
        });

    let man = moved_ship.ship.moved_distance();
    Ok(man.to_string())
}
