use crate::*;

pub const EMPTY_TILE: u8 = 0;

pub struct GridSolver {
    grid: Grid,
    solutions: Vec<Grid>,
}

impl GridSolver {
    pub fn new(grid: Grid) -> Self {
        GridSolver {
            grid,
            solutions: Vec::new(),
        }
    }

    pub fn solve(mut self) -> Vec<Grid> {
        let Grid(tiles) = self.grid;
        self.solve_iter(0, tiles);
        self.solutions
    }

    fn solve_iter(&mut self, index: usize, mut tiles: [u8; 81]) {
        if index == TILES_PER_GRID {
            self.solutions.push(Grid(tiles));
            return;
        }

        if tiles[index] != EMPTY_TILE {
            self.solve_iter(index + 1, tiles);
            return;
        }

        for value in 1..=9 {
            if is_value_possible(value, index, &tiles) {
                self.solve_iter(index + 1, {
                    tiles[index] = value;
                    tiles
                });
            }
        }
    }
}

fn is_value_possible(value: u8, index: usize, tiles: &[u8; TILES_PER_GRID]) -> bool {
    is_row_valid(value, index, tiles)
        && is_column_valid(value, index, tiles)
        && is_block_valid(value, index, tiles)
}

fn is_row_valid(value: u8, index: usize, tiles: &[u8; TILES_PER_GRID]) -> bool {
    let relative_index = index / TILES_PER_GRID_SIDE;
    let mut indices = (0..TILES_PER_GRID_SIDE).map(|i| relative_index * TILES_PER_GRID_SIDE + i);
    is_value_not_taken(&mut indices, value, tiles)
}

fn is_column_valid(value: u8, index: usize, tiles: &[u8; TILES_PER_GRID]) -> bool {
    let relative_index = index % TILES_PER_GRID_SIDE;
    let mut indices = (0..TILES_PER_GRID_SIDE).map(|i| i * TILES_PER_GRID_SIDE + relative_index);
    is_value_not_taken(&mut indices, value, tiles)
}

fn is_block_valid(value: u8, index: usize, tiles: &[u8; TILES_PER_GRID]) -> bool {
    let block_x = (index % TILES_PER_GRID_SIDE) / TILES_PER_BLOCK_SIDE;
    let block_y = (index / TILES_PER_GRID_SIDE) / TILES_PER_BLOCK_SIDE;
    let first_index =
        TILES_PER_BLOCK_SIDE * TILES_PER_GRID_SIDE * block_y + TILES_PER_BLOCK_SIDE * block_x;
    let mut indices = (0..TILES_PER_BLOCK_SIDE).flat_map(|i| {
        (0..TILES_PER_BLOCK_SIDE).map(move |column_offset| {
            let row_offset = i * TILES_PER_GRID_SIDE;
            first_index + row_offset + column_offset
        })
    });
    is_value_not_taken(&mut indices, value, tiles)
}

fn is_value_not_taken<I>(indices: &mut I, value: u8, tiles: &[u8; TILES_PER_GRID]) -> bool
where
    I: Iterator<Item = usize>,
{
    indices.all(|absolute_index| tiles[absolute_index] != value)
}

#[cfg(test)]
mod grid_solver_tests {
    use rstest::*;

    use crate::grid_solver::*;

    #[fixture]
    fn row_and_column_grid() -> Grid {
        test_util::build_grid([
            [0, 1, 0, 0, 2, 0, 0, 3, 0],
            [5, 0, 6, 7, 3, 8, 9, 4, 1],
            [0, 3, 0, 0, 4, 0, 0, 5, 0],
            [0, 4, 0, 0, 5, 0, 0, 6, 0],
            [8, 5, 9, 1, 0, 2, 3, 7, 4],
            [0, 6, 0, 0, 7, 0, 0, 8, 0],
            [0, 7, 0, 0, 8, 0, 0, 9, 0],
            [2, 8, 3, 4, 9, 5, 6, 0, 7],
            [0, 9, 0, 0, 1, 0, 0, 2, 0],
        ])
    }

    #[fixture]
    fn block_grid() -> Grid {
        test_util::build_grid([
            [0, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 5, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 5, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 0],
        ])
    }

    #[rstest]
    #[case(true, 2, 10)]
    #[case(true, 6, 40)]
    #[case(true, 1, 70)]
    #[case(false, 3, 10)]
    #[case(false, 7, 40)]
    #[case(false, 8, 70)]
    fn is_row_valid_test(
        row_and_column_grid: Grid,
        #[case] expected: bool,
        #[case] value: u8,
        #[case] index: usize,
    ) {
        let Grid(tiles) = row_and_column_grid;

        let actual = is_column_valid(value, index, &tiles);

        assert_eq!(expected, actual);
    }

    #[rstest]
    #[case(true, 2, 10)]
    #[case(true, 6, 40)]
    #[case(true, 1, 70)]
    #[case(false, 5, 10)]
    #[case(false, 9, 40)]
    #[case(false, 4, 70)]
    fn is_column_valid_test(
        row_and_column_grid: Grid,
        #[case] expected: bool,
        #[case] value: u8,
        #[case] index: usize,
    ) {
        let Grid(tiles) = row_and_column_grid;

        let actual = is_column_valid(value, index, &tiles);

        assert_eq!(expected, actual);
    }

    #[rstest]
    #[case(true, 1, 0)]
    #[case(true, 5, 40)]
    #[case(true, 9, 80)]
    #[case(false, 5, 0)]
    #[case(false, 9, 40)]
    #[case(false, 1, 80)]
    fn is_block_valid_test(
        block_grid: Grid,
        #[case] expected: bool,
        #[case] value: u8,
        #[case] index: usize,
    ) {
        let Grid(tiles) = block_grid;

        let actual = is_block_valid(value, index, &tiles);

        assert_eq!(expected, actual);
    }
}
