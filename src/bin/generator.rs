use std::fmt::Display;

use rand::{rngs::StdRng, SeedableRng};

fn main() {
    println!("Hello World!");

    let grid_generator = GridGenerator::with_seed(1);

    let grid = grid_generator.create(17);
    println!("{}", grid);
    println!("{}", grid.draw().join(""));

    // grid is empty
    // loops (at least 17 times):
    // - select a random tile
    // - find a valid number
    // - fill it in
    // - remove tile in list, in order to not pick it again
}

const NUMBER_OF_TILES: usize = 9 * 9;

struct GridGenerator {
    prng: StdRng,
}

impl GridGenerator {
    fn with_seed(seed: u64) -> Self {
        Self {
            prng: StdRng::seed_from_u64(seed),
        }
    }

    fn create(&self, given_tiles: usize) -> Grid {
        Grid([0; NUMBER_OF_TILES])
    }
}

struct Grid([u8; NUMBER_OF_TILES]);

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Grid {
    fn draw(&self) -> Vec<String> {
        todo!()
    }
}

#[cfg(test)]
mod grid_tests {
    use super::*;

    #[test]
    fn create_test() {
        assert!(true);
    }
}
