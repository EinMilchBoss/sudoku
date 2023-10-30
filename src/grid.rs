use std::{fmt, str};

use itertools::{Either, Itertools};
use thiserror::Error;

use crate::GridSolver;

pub const TILES_PER_GRID: usize = TILES_PER_GRID_SIDE * TILES_PER_GRID_SIDE;
pub const TILES_PER_GRID_SIDE: usize = 9;
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

        let (values, errors): (Vec<_>, Vec<_>) = s
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

        if errors.is_empty() {
            Ok(Grid(values.try_into().unwrap()))
        } else {
            Err(ParseGridError::InvalidChars(errors))
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParseGridError {
    #[error("One or more chars were invalid.")]
    InvalidChars(Vec<InvalidChar>),
    #[error("Expected an input of size {}, found {} instead.", TILES_PER_GRID, .0)]
    InvalidLength(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidChar {
    index: usize,
    invalid_char: char,
}

impl InvalidChar {
    pub fn to_bullet_point(&self) -> String {
        format!(
            "- Invalid char '{}' at position {}.",
            self.invalid_char, self.index
        )
    }
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

    fn extract_errors(parsed: Result<Grid, ParseGridError>) -> ParseGridError {
        parsed.expect_err("Parsing should have failed.")
    }
}
