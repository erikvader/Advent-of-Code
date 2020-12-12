use crate::parsers as P;
use anyhow::Context;
use grid::Grid;
use petgraph::graph::{NodeIndex, UnGraph};
use std::collections::HashMap;

#[derive(Debug)]
struct Seats {
    graph: UnGraph<bool, ()>,
    coords: HashMap<(usize, usize), NodeIndex<u32>>,
}

impl Seats {
    fn new(g: &Grid<char>) -> Self {
        let mut graph = UnGraph::new_undirected();
        let mut coords = HashMap::new();

        for r in 0..g.rows() {
            for c in 0..g.cols() {
                let ch = *g.get(r, c).unwrap();
                if ch == '.' {
                    continue;
                }
                let i = graph.add_node(ch == '#');
                coords.insert((r, c), i);
            }
        }

        Self { graph, coords }
    }

    fn fill_adj(&mut self) {
        for (pos, ni) in self.coords.iter() {
            let ir = pos.0 as isize;
            let ic = pos.1 as isize;

            for r in -1..=1 {
                for c in -1..=1 {
                    if r == 0 && c == 0 {
                        continue;
                    }
                    let nr = ir + r;
                    let nc = ic + c;
                    if nr < 0 || nc < 0 {
                        continue;
                    }
                    if let Some(neigh) = self.coords.get(&(nr as usize, nc as usize)) {
                        self.graph.update_edge(*ni, *neigh, ());
                    }
                }
            }
        }
    }

    fn simulate(&mut self, seat_level: u32) {
        let mut buf = self.graph.clone();

        let mut changed = true;
        while changed {
            changed = false;
            for n in self.graph.node_indices() {
                let noos = self.seated_neigh(n);
                let empty = !self.graph[n];

                if empty && noos == 0 {
                    buf[n] = true;
                    changed = true;
                } else if !empty && noos >= seat_level {
                    buf[n] = false;
                    changed = true;
                } else {
                    buf[n] = self.graph[n]
                }
            }

            std::mem::swap(&mut self.graph, &mut buf);
        }
    }

    fn seated(&self) -> usize {
        self.graph
            .node_indices()
            .map(|x| self.graph[x])
            .filter(|s| *s)
            .count()
    }

    fn seated_neigh(&self, n: NodeIndex<u32>) -> u32 {
        let mut adj = 0;
        for neigh in self.graph.neighbors(n) {
            if self.graph[neigh] {
                adj += 1;
            }
        }
        adj
    }

    fn fill_line(&mut self, g: &Grid<char>) {
        for (pos, ni) in self.coords.iter() {
            for r in -1..=1 {
                for c in -1..=1 {
                    if r == 0 && c == 0 {
                        continue;
                    }
                    if let Some(neigh) = long_neigh(g, pos.0, pos.1, r, c) {
                        let t = self.coords.get(&neigh).unwrap();
                        self.graph.update_edge(*ni, *t, ());
                    }
                }
            }
        }
    }
}

fn long_neigh(
    g: &Grid<char>,
    row: usize,
    col: usize,
    dr: isize,
    dc: isize,
) -> Option<(usize, usize)> {
    let mut cr = row as isize;
    let mut cc = col as isize;

    loop {
        cr += dr;
        cc += dc;

        if cr < 0 || cc < 0 {
            return None;
        }

        let tmp = g.get(cr as usize, cc as usize)?;

        if *tmp == '.' {
            continue;
        }

        return Some((cr as usize, cc as usize));
    }
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let g = P::char_grid(input).context("couldn't parse seating")?;

    let mut seats = Seats::new(&g);
    seats.fill_adj();
    seats.simulate(4);

    Ok(seats.seated().to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let g = P::char_grid(input).context("couldn't parse seating")?;

    let mut seats = Seats::new(&g);
    seats.fill_line(&g);
    seats.simulate(5);

    Ok(seats.seated().to_string())
}
