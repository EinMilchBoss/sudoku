use std::{fmt, str};

use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{GridSolver, EMPTY_TILE};

pub const TILES_PER_GRID: usize = TILES_PER_GRID_SIDE * TILES_PER_GRID_SIDE;
pub const TILES_PER_GRID_SIDE: usize = 9;
pub const TILES_PER_BLOCK: usize = TILES_PER_BLOCK_SIDE * TILES_PER_BLOCK_SIDE;
pub const TILES_PER_BLOCK_SIDE: usize = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Grid(pub [u8; TILES_PER_GRID]);

impl Grid {
    pub fn solve(&self) -> Vec<Grid> {
        GridSolver::new(*self).solve()
    }

    pub fn to_pretty_string(&self) -> String {
        let Self(values) = self;
        values
            .chunks(TILES_PER_BLOCK_SIDE * TILES_PER_GRID_SIDE)
            .map(|layer| {
                layer
                    .chunks(TILES_PER_GRID_SIDE)
                    .map(|row| {
                        row.chunks(TILES_PER_BLOCK_SIDE)
                            .map(|block_row| block_row.iter().join(" "))
                            .join(" | ")
                    })
                    .join("\n")
            })
            .join("\n------+-------+------\n")
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(tiles) = self;
        write!(f, "{}", tiles.iter().join(""))
    }
}

impl str::FromStr for Grid {
    type Err = ParseGridError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let length = s.chars().count();
        if length != TILES_PER_GRID {
            return Err(ParseGridError::InvalidLength(length));
        }

        let (values, invalid_chars): (Vec<_>, Vec<_>) = s
            .chars()
            .enumerate()
            .map(|(index, char)| {
                char.to_digit(10).ok_or(InvalidChar {
                    index,
                    invalid_char: char,
                })
            })
            .partition_map(|result| match result {
                Ok(value) => Either::Left(value as u8),
                Err(error) => Either::Right(error),
            });

        if !invalid_chars.is_empty() {
            return Err(ParseGridError::InvalidChars(invalid_chars));
        }

        let invalid_areas = invalid_rows(&values)
            .chain(invalid_columns(&values))
            .chain(invalid_blocks(&values))
            .collect_vec();

        if !invalid_areas.is_empty() {
            return Err(ParseGridError::InvalidAreas(invalid_areas));
        }

        Ok(Grid(values.try_into().unwrap()))
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParseGridError {
    #[error("One or more chars were invalid.")]
    InvalidChars(Vec<InvalidChar>),
    #[error("One or more areas (rows, columns or blocks) were invalid.")]
    InvalidAreas(Vec<InvalidArea>),
    #[error("Expected an input of size {}, found {} instead.", TILES_PER_GRID, .0)]
    InvalidLength(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidChar {
    pub index: usize,
    pub invalid_char: char,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InvalidArea {
    Row(InvalidTile),
    Column(InvalidTile),
    Block(InvalidTile),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidTile {
    pub index: usize,
    pub value: u8,
}

fn invalid_rows(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    row_tiles_iter(tiles)
        .enumerate()
        .flat_map(invalid_element)
        .map(InvalidArea::Row)
}

fn invalid_columns(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    column_tiles_iter(tiles)
        .enumerate()
        .flat_map(invalid_element)
        .map(InvalidArea::Column)
}

fn invalid_blocks(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    block_tiles_iter(tiles)
        .enumerate()
        .flat_map(invalid_element)
        .map(InvalidArea::Block)
}

fn row_tiles_iter(tiles: &[u8]) -> impl Iterator<Item = impl Iterator<Item = u8> + '_> {
    (0..TILES_PER_GRID_SIDE).map(move |column_offset| {
        (0..TILES_PER_GRID_SIDE)
            .map(move |row_offset| tiles[column_offset * TILES_PER_GRID_SIDE + row_offset])
    })
}

fn column_tiles_iter(tiles: &[u8]) -> impl Iterator<Item = impl Iterator<Item = u8> + '_> {
    (0..TILES_PER_GRID_SIDE).map(move |column_offset| {
        (0..TILES_PER_GRID_SIDE)
            .map(move |row_offset| tiles[row_offset * TILES_PER_GRID_SIDE + column_offset])
    })
}

fn block_tiles_iter(tiles: &[u8]) -> impl Iterator<Item = impl Iterator<Item = u8> + '_> {
    (0..TILES_PER_BLOCK_SIDE).flat_map(move |block_x| {
        (0..TILES_PER_BLOCK_SIDE).map(move |block_y| {
            (0..TILES_PER_BLOCK).map(move |i| {
                let fix_column_offset = TILES_PER_BLOCK_SIDE * block_x;
                let fix_row_offset = TILES_PER_BLOCK_SIDE * block_y * TILES_PER_GRID_SIDE;
                let fix_offset = fix_column_offset + fix_row_offset;
                let column_offset = i % TILES_PER_BLOCK_SIDE;
                let row_offset = i / TILES_PER_BLOCK_SIDE * TILES_PER_GRID_SIDE;
                tiles[fix_offset + row_offset + column_offset]
            })
        })
    })
}

fn invalid_element(
    (index, area): (usize, impl Iterator<Item = u8>),
) -> impl Iterator<Item = InvalidTile> {
    let occurrences = area.counts_by(|value| value);
    occurrences.into_iter().filter_map(move |(value, count)| {
        if count > 1 && value != EMPTY_TILE {
            Some(InvalidTile { index, value })
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};

    use crate::{grid::*, test_util, Grid};

    #[fixture]
    fn grid() -> Grid {
        test_util::build_grid([
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [2, 3, 4, 5, 6, 7, 8, 9, 1],
            [0, 0, 5, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 7, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 9, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 1, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 3, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 5, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 7],
        ])
    }

    #[rstest]
    fn row_tiles_iter_test(grid: Grid) {
        let Grid(tiles) = grid;
        let expected = vec![
            vec![1, 2, 3, 4, 5, 6, 7, 8, 9],
            vec![2, 3, 4, 5, 6, 7, 8, 9, 1],
            vec![0, 0, 5, 0, 0, 0, 0, 0, 0],
            vec![0, 0, 0, 7, 0, 0, 0, 0, 0],
            vec![0, 0, 0, 0, 9, 0, 0, 0, 0],
            vec![0, 0, 0, 0, 0, 1, 0, 0, 0],
            vec![0, 0, 0, 0, 0, 0, 3, 0, 0],
            vec![0, 0, 0, 0, 0, 0, 0, 5, 0],
            vec![0, 0, 0, 0, 0, 0, 0, 0, 7],
        ];

        let actual = row_tiles_iter(&tiles)
            .map(|iter| iter.collect_vec())
            .collect_vec();

        assert_eq!(expected, actual);
    }

    #[rstest]
    fn column_tiles_iter_test(grid: Grid) {
        let Grid(tiles) = grid;
        let expected = vec![
            vec![1, 2, 0, 0, 0, 0, 0, 0, 0],
            vec![2, 3, 0, 0, 0, 0, 0, 0, 0],
            vec![3, 4, 5, 0, 0, 0, 0, 0, 0],
            vec![4, 5, 0, 7, 0, 0, 0, 0, 0],
            vec![5, 6, 0, 0, 9, 0, 0, 0, 0],
            vec![6, 7, 0, 0, 0, 1, 0, 0, 0],
            vec![7, 8, 0, 0, 0, 0, 3, 0, 0],
            vec![8, 9, 0, 0, 0, 0, 0, 5, 0],
            vec![9, 1, 0, 0, 0, 0, 0, 0, 7],
        ];

        let actual = column_tiles_iter(&tiles)
            .map(|iter| iter.collect_vec())
            .collect_vec();

        assert_eq!(expected, actual);
    }

    #[rstest]
    fn block_tiles_iter_test(grid: Grid) {
        let Grid(tiles) = grid;
        let expected = vec![
            vec![1, 2, 3, 2, 3, 4, 0, 0, 5],
            vec![0, 0, 0, 0, 0, 0, 0, 0, 0],
            vec![0, 0, 0, 0, 0, 0, 0, 0, 0],
            vec![4, 5, 6, 5, 6, 7, 0, 0, 0],
            vec![7, 0, 0, 0, 9, 0, 0, 0, 1],
            vec![0, 0, 0, 0, 0, 0, 0, 0, 0],
            vec![7, 8, 9, 8, 9, 1, 0, 0, 0],
            vec![0, 0, 0, 0, 0, 0, 0, 0, 0],
            vec![3, 0, 0, 0, 5, 0, 0, 0, 7],
        ];

        let actual = block_tiles_iter(&tiles)
            .map(|iter| iter.collect_vec())
            .collect_vec();

        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod grid_tests {
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};

    use crate::*;

    #[fixture]
    pub fn grid() -> Grid {
        test_util::build_grid([
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [9, 8, 7, 6, 5, 4, 3, 2, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [6, 6, 6, 6, 6, 6, 6, 6, 6],
            [7, 7, 7, 7, 7, 7, 7, 7, 7],
            [8, 8, 8, 8, 8, 8, 8, 8, 8],
            [9, 9, 9, 9, 9, 9, 9, 9, 9],
            [1, 1, 1, 1, 1, 1, 1, 1, 1],
        ])
    }

    #[rstest]
    fn to_pretty_string_test(grid: Grid) {
        let expected = [
            "1 2 3 | 4 5 6 | 7 8 9",
            "0 0 0 | 0 0 0 | 0 0 0",
            "9 8 7 | 6 5 4 | 3 2 1",
            "------+-------+------",
            "0 0 0 | 0 0 0 | 0 0 0",
            "6 6 6 | 6 6 6 | 6 6 6",
            "7 7 7 | 7 7 7 | 7 7 7",
            "------+-------+------",
            "8 8 8 | 8 8 8 | 8 8 8",
            "9 9 9 | 9 9 9 | 9 9 9",
            "1 1 1 | 1 1 1 | 1 1 1",
        ]
        .join("\n");

        let actual = grid.to_pretty_string();

        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod grid_display_trait_tests {
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};

    use crate::*;

    #[fixture]
    pub fn grid() -> Grid {
        test_util::build_grid([
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [9, 8, 7, 6, 5, 4, 3, 2, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [6, 6, 6, 6, 6, 6, 6, 6, 6],
            [7, 7, 7, 7, 7, 7, 7, 7, 7],
            [8, 8, 8, 8, 8, 8, 8, 8, 8],
            [9, 9, 9, 9, 9, 9, 9, 9, 9],
            [1, 1, 1, 1, 1, 1, 1, 1, 1],
        ])
    }

    #[rstest]
    fn to_string_test(grid: Grid) {
        let expected =
            "123456789000000000987654321000000000666666666777777777888888888999999999111111111";

        let actual = grid.to_string();

        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod grid_from_str_trait_tests {
    use itertools::Itertools;
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};

    use crate::*;

    #[fixture]
    fn tiles() -> [[u8; TILES_PER_GRID_SIDE]; TILES_PER_GRID_SIDE] {
        [
            [0, 1, 0, 0, 2, 0, 0, 3, 0],
            [5, 0, 6, 7, 3, 8, 9, 4, 1],
            [0, 3, 0, 0, 4, 0, 0, 5, 0],
            [0, 4, 0, 0, 5, 0, 0, 6, 0],
            [8, 5, 9, 1, 0, 2, 3, 7, 4],
            [0, 6, 0, 0, 7, 0, 0, 8, 0],
            [0, 7, 0, 0, 8, 0, 0, 9, 0],
            [2, 8, 3, 4, 9, 5, 6, 0, 7],
            [0, 9, 0, 0, 1, 0, 0, 2, 0],
        ]
    }

    #[rstest]
    fn from_str_test_valid_chars(tiles: [[u8; TILES_PER_GRID_SIDE]; TILES_PER_GRID_SIDE]) {
        let input = tiles.map(|row| row.into_iter().join("")).join("");
        let expected = test_util::build_grid(tiles);

        let actual = input.parse::<Grid>();

        assert_eq!(Ok(expected), actual);
    }

    #[rstest]
    fn from_str_test_invalid_chars(tiles: [[u8; TILES_PER_GRID_SIDE]; TILES_PER_GRID_SIDE]) {
        let invalid_char_amount = 1;
        let bad_input = tiles.map(|row| row.into_iter().join("")).join("").replacen(
            '1',
            "a",
            invalid_char_amount,
        );

        let error = extract_error(bad_input.parse::<Grid>());

        match error {
            ParseGridError::InvalidChars(invalid_chars) => {
                assert_eq!(invalid_char_amount, invalid_chars.len());
            }
            _ => panic!("Expected another error."),
        }
    }

    #[test]
    fn from_str_test_right_amount() {
        let input_empty_grid = "0".repeat(TILES_PER_GRID);

        let grid = input_empty_grid.parse();

        assert_eq!(Ok(Grid([0; TILES_PER_GRID])), grid);
    }

    #[test]
    fn from_str_test_too_big() {
        let input_length = TILES_PER_GRID + 1;
        let input_bigger = "0".repeat(input_length);

        let error = extract_error(input_bigger.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            _ => panic!("Expected another error."),
        }
    }

    #[test]
    fn from_str_test_too_small() {
        let input_length = TILES_PER_GRID - 1;
        let input_smaller = "0".repeat(input_length);

        let error = extract_error(input_smaller.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            _ => panic!("Expected another error."),
        }
    }

    #[test]
    fn from_str_test_invalid_areas() {
        let input = "123456789".repeat(TILES_PER_GRID_SIDE);

        let error = extract_error(input.parse::<Grid>());

        match error {
            ParseGridError::InvalidAreas(invalid_areas) => {
                assert_eq!(true, !invalid_areas.is_empty());
            }
            _ => panic!("Expected another error."),
        }
    }

    fn extract_error(parsed: Result<Grid, ParseGridError>) -> ParseGridError {
        parsed.expect_err("Parsing should have failed.")
    }
}
