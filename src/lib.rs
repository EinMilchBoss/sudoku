mod grid;
mod grid_solver;

pub use grid::*;
pub use grid_solver::*;

#[cfg(test)]
mod test_util {
    use crate::*;

    pub fn build_grid(lines: [[u8; TILES_PER_GRID_SIDE]; TILES_PER_GRID_SIDE]) -> Grid {
        let tiles = lines
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        Grid(tiles)
    }
}
