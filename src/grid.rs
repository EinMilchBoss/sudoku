use std::{collections::HashSet, fmt, str};

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

fn invalid_blocks(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    let block_tiles_iter = (0..TILES_PER_BLOCK_SIDE).flat_map(move |column_offset| {
        (0..TILES_PER_BLOCK_SIDE).map(move |row_offset| {
            (0..TILES_PER_BLOCK)
                .map(move |i| {
                    let block_row_offset = i / 3;
                    let fix_offset = row_offset * TILES_PER_BLOCK_SIDE + column_offset;
                    fix_offset + block_row_offset * TILES_PER_BLOCK_SIDE + block_row_offset
                })
                .map(move |index| tiles[index])
        })
    });

    invalid_elements(block_tiles_iter, &InvalidArea::Block)
}

fn invalid_columns(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    let column_tiles_iter = (0..TILES_PER_GRID_SIDE).map(move |column_offset| {
        (0..TILES_PER_GRID_SIDE)
            .map(move |row_offset| tiles[row_offset * TILES_PER_GRID_SIDE + column_offset])
    });

    invalid_elements(column_tiles_iter, &InvalidArea::Column)
}

fn invalid_rows(tiles: &[u8]) -> impl Iterator<Item = InvalidArea> + '_ {
    let row_tiles_iter = (0..TILES_PER_GRID_SIDE).map(move |column_offset| {
        (0..TILES_PER_GRID_SIDE)
            .map(move |row_offset| tiles[column_offset * TILES_PER_GRID_SIDE + row_offset])
    });

    invalid_elements(row_tiles_iter, &InvalidArea::Row)
}

fn invalid_elements<'a, II, IO, F>(
    tiles_iters: IO,
    to_error_element: &'a F,
) -> impl Iterator<Item = InvalidArea> + 'a
where
    II: Iterator<Item = u8> + 'a,
    IO: Iterator<Item = II> + 'a,
    F: Fn(InvalidTile) -> InvalidArea + 'a,
{
    tiles_iters
        .enumerate()
        .flat_map(move |(index, tiles_iter)| invalid_element(index, tiles_iter, to_error_element))
}

fn invalid_element<F, I>(
    index: usize,
    area: I,
    to_error_element: F,
) -> impl Iterator<Item = InvalidArea>
where
    I: Iterator<Item = u8>,
    F: Fn(InvalidTile) -> InvalidArea,
{
    let occurrences = area.counts_by(|value| value);
    occurrences.into_iter().filter_map(move |(value, count)| {
        if count > 1 && value != EMPTY_TILE {
            Some(to_error_element(InvalidTile { index, value }))
        } else {
            None
        }
    })
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
pub enum InvalidArea {
    Row(InvalidTile),
    Column(InvalidTile),
    Block(InvalidTile),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidTile {
    index: usize,
    value: u8,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidChar {
    pub index: usize,
    pub invalid_char: char,
}

#[cfg(test)]
mod test_util {
    use rstest::fixture;

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
}

#[cfg(test)]
mod grid_tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::{grid::test_util::grid, *};

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
    use rstest::rstest;

    use crate::{grid::test_util::grid, *};

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
    use pretty_assertions::assert_eq;

    use crate::*;

    #[test]
    fn from_str_test_valid_chars() {
        let input_valid_chars = ["123456789"; 9].join("");
        let expected = test_util::build_grid([[1, 2, 3, 4, 5, 6, 7, 8, 9]; 9]);

        let actual = input_valid_chars.parse::<Grid>();

        assert_eq!(Ok(expected), actual);
    }

    #[test]
    fn from_str_test_invalid_chars() {
        let input_valid_chars = ["a23456789"; 9].join("");

        let error = extract_errors(input_valid_chars.parse::<Grid>());

        match error {
            ParseGridError::InvalidChars(invalid_chars) => {
                assert_eq!(9, invalid_chars.len());
            }
            _ => panic!("Expected an `InvalidChars` error."),
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

        let error = extract_errors(input_bigger.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            _ => panic!("Expected an `InvalidLength` error."),
        }
    }

    #[test]
    fn from_str_test_too_small() {
        let input_length = TILES_PER_GRID - 1;
        let input_smaller: String = "0".repeat(input_length);

        let error = extract_errors(input_smaller.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            _ => panic!("Expected an `InvalidLength` error."),
        }
    }

    // #[test]
    // fn from_str_test_invalid_values() {
    //     let input_length = TILES_PER_GRID - 1;
    //     let input_smaller: String = "0".repeat(input_length);

    //     let error = extract_errors(input_smaller.parse::<Grid>());

    //     match error {
    //         ParseGridError::InvalidValues(invalid_values) => {
    //             assert_eq!(input_length, actual_length);
    //         }
    //         _ => panic!("Expected an `InvalidValues` error."),
    //     }
    // }

    fn extract_errors(parsed: Result<Grid, ParseGridError>) -> ParseGridError {
        parsed.expect_err("Parsing should have failed.")
    }
}
