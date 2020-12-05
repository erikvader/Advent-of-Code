use crate::parsers as P;
use anyhow;
use grid::Grid;

fn count_trees(g: &Grid<char>, dr: usize, dc: usize) -> usize {
    let mut r = 0;
    let mut c = 0;

    let mut trees = 0;
    while r < g.rows() {
        if *g.get(r, c).unwrap() == '#' {
            trees += 1;
        }

        r += dr;
        c = (c + dc) % g.cols();
    }

    trees
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let g = P::char_grid(input)?;

    Ok(count_trees(&g, 1, 3).to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let g = P::char_grid(input)?;

    let res = count_trees(&g, 1, 1)
        * count_trees(&g, 1, 3)
        * count_trees(&g, 1, 5)
        * count_trees(&g, 1, 7)
        * count_trees(&g, 2, 1);

    Ok(res.to_string())
}
