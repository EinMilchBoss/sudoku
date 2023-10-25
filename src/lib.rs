use std::{fmt, str::FromStr};

use itertools::{Either, Itertools};
use thiserror::Error;

const VALUES_PER_GRID: usize = VALUES_PER_GRID_SIDE * VALUES_PER_GRID_SIDE;
const VALUES_PER_GRID_SIDE: usize = 9;
const VALUES_PER_BLOCK: usize = VALUES_PER_BLOCK_SIDE * VALUES_PER_BLOCK_SIDE;
const VALUES_PER_BLOCK_SIDE: usize = 3;
const EMPTY_TILE: u8 = 0;

#[derive(Debug, PartialEq, Eq)]
pub struct Grid([u8; VALUES_PER_GRID]);

impl Grid {
    pub fn solve_grid(&self) -> Option<Grid> {
        let Grid(tiles) = self;

        solve_grid_iter(0, *tiles)
    }
}

fn solve_grid_iter(index: usize, mut tiles: [u8; 81]) -> Option<Grid> {
    if index == VALUES_PER_GRID {
        return Some(Grid(tiles));
    }

    if tiles[index] == EMPTY_TILE {
        for value in 1..=9 {
            if is_value_possible(value, index, &tiles) {
                let next = solve_grid_iter(index + 1, {
                    tiles[index] = value;
                    tiles
                });
                if next.is_some() {
                    return next;
                }
            }
        }
        None
    } else {
        solve_grid_iter(index + 1, tiles)
    }
}

fn is_value_possible(value: u8, index: usize, tiles: &[u8; VALUES_PER_GRID]) -> bool {
    is_row_valid(value, index, tiles)
        && is_column_valid(value, index, tiles)
        && is_block_valid(value, index, tiles)
}

fn is_column_valid(value: u8, index: usize, tiles: &[u8; VALUES_PER_GRID]) -> bool {
    let relative_index = index % VALUES_PER_GRID_SIDE;
    (0..VALUES_PER_GRID_SIDE)
        .map(|i| i * VALUES_PER_GRID_SIDE + relative_index)
        .all(|absolute_index| tiles[absolute_index] != value)
}

fn is_row_valid(value: u8, index: usize, tiles: &[u8; VALUES_PER_GRID]) -> bool {
    let relative_index = index / VALUES_PER_GRID_SIDE;
    (0..VALUES_PER_GRID_SIDE)
        .map(|i| relative_index * VALUES_PER_GRID_SIDE + i)
        .all(|absolute_index| tiles[absolute_index] != value)
}

fn is_block_valid(value: u8, index: usize, tiles: &[u8; VALUES_PER_GRID]) -> bool {
    let block_x = (index % VALUES_PER_GRID_SIDE) / VALUES_PER_BLOCK_SIDE;
    let block_y = (index / VALUES_PER_GRID_SIDE) / VALUES_PER_BLOCK_SIDE;
    let first_index =
        VALUES_PER_BLOCK_SIDE * VALUES_PER_GRID_SIDE * block_y + VALUES_PER_BLOCK_SIDE * block_x;
    (0..VALUES_PER_BLOCK_SIDE)
        .flat_map(|i| {
            (0..VALUES_PER_BLOCK_SIDE).map(move |column_offset| {
                let row_offset = i * VALUES_PER_GRID_SIDE;
                first_index + row_offset + column_offset
            })
        })
        .all(|absolute_index| tiles[absolute_index] != value)
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(values) = self;
        let content = values
            .chunks(VALUES_PER_BLOCK_SIDE * VALUES_PER_GRID_SIDE)
            .map(|layer| {
                layer
                    .chunks(VALUES_PER_GRID_SIDE)
                    .map(|row| {
                        row.chunks(VALUES_PER_BLOCK_SIDE)
                            .map(|block_row| block_row.iter().join(" "))
                            .join(" | ")
                    })
                    .join("\n")
            })
            .join("\n------+-------+------\n");
        write!(f, "{}", content)
    }
}

impl FromStr for Grid {
    type Err = ParseGridError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let length = s.chars().count();
        if length != VALUES_PER_GRID {
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
    #[error("Expected an input of size {}, found {} instead.", VALUES_PER_GRID, .0)]
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
mod grid_solve_tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn is_block_valid_test_true() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_block_valid(10, 5, &input);
        let second = is_block_valid(40, 5, &input);
        let third = is_block_valid(70, 5, &input);

        assert_eq!(true, first);
        assert_eq!(true, second);
        assert_eq!(true, third);
    }

    #[test]
    fn is_block_valid_test_false() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_block_valid(10, 1, &input);
        let second = is_block_valid(40, 6, &input);
        let third = is_block_valid(70, 8, &input);

        assert_eq!(false, first);
        assert_eq!(false, second);
        assert_eq!(false, third);
    }

    #[test]
    fn is_col_valid_test_true() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_column_valid(10, 5, &input);
        let second = is_column_valid(40, 5, &input);
        let third = is_column_valid(70, 5, &input);

        assert_eq!(true, first);
        assert_eq!(true, second);
        assert_eq!(true, third);
    }

    #[test]
    fn is_col_valid_test_false() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_column_valid(10, 2, &input);
        let second = is_column_valid(40, 8, &input);
        let third = is_column_valid(70, 2, &input);

        assert_eq!(false, first);
        assert_eq!(false, second);
        assert_eq!(false, third);
    }

    #[test]
    fn is_row_valid_test_true() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_row_valid(10, 5, &input);
        let second = is_row_valid(40, 5, &input);
        let third = is_row_valid(70, 5, &input);

        assert_eq!(true, first);
        assert_eq!(true, second);
        assert_eq!(true, third);
    }

    #[test]
    fn is_row_valid_test_false() {
        let input = vec![
            [1, 2, 3, 0, 0, 0, 0, 0, 0],
            [4, 0, 6, 0, 0, 0, 0, 0, 0],
            [7, 8, 9, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 1, 2, 3, 0, 0, 0],
            [0, 0, 0, 4, 0, 6, 0, 0, 0],
            [0, 0, 0, 7, 8, 9, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 2, 3],
            [0, 0, 0, 0, 0, 0, 4, 0, 6],
            [0, 0, 0, 0, 0, 0, 7, 8, 9],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

        let first = is_row_valid(10, 4, &input);
        let second = is_row_valid(40, 6, &input);
        let third = is_row_valid(70, 4, &input);

        assert_eq!(false, first);
        assert_eq!(false, second);
        assert_eq!(false, third);
    }
}

#[cfg(test)]
mod grid_display_trait_tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn to_string_test() {
        let input = vec![
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [9, 8, 7, 6, 5, 4, 3, 2, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [6, 6, 6, 6, 6, 6, 6, 6, 6],
            [7, 7, 7, 7, 7, 7, 7, 7, 7],
            [8, 8, 8, 8, 8, 8, 8, 8, 8],
            [9, 9, 9, 9, 9, 9, 9, 9, 9],
            [1, 1, 1, 1, 1, 1, 1, 1, 1],
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
        let grid = Grid(input);
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

        let actual = grid.to_string();

        assert_eq!(expected, actual)
    }
}

#[cfg(test)]
mod grid_from_str_trait_tests {
    use super::*;

    #[test]
    fn from_str_test_valid_chars() {
        let input_valid_chars = ["123456789"; 9].join("");
        let expected = Grid(
            vec![[1, 2, 3, 4, 5, 6, 7, 8, 9]; 9]
                .into_iter()
                .flatten()
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        );

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
            ParseGridError::InvalidLength(_) => panic!("Expected an `InvalidChars` error."),
        }
    }

    #[test]
    fn from_str_test_right_amount() {
        let input_empty_grid: String = "0".repeat(VALUES_PER_GRID);

        let grid = input_empty_grid.parse();

        assert_eq!(Ok(Grid([0; VALUES_PER_GRID])), grid);
    }

    #[test]
    fn from_str_test_too_big() {
        let input_length = VALUES_PER_GRID + 1;
        let input_bigger: String = "0".repeat(input_length);

        let error = extract_errors(input_bigger.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            ParseGridError::InvalidChars(_) => panic!("Expected an `InvalidLength` error."),
        }
    }

    #[test]
    fn from_str_test_too_small() {
        let input_length = VALUES_PER_GRID - 1;
        let input_smaller: String = "0".repeat(input_length);

        let error = extract_errors(input_smaller.parse::<Grid>());

        match error {
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            ParseGridError::InvalidChars(_) => panic!("Expected an `InvalidLength` error."),
        }
    }

    fn extract_errors(parsed: Result<Grid, ParseGridError>) -> ParseGridError {
        parsed.expect_err("Parsing should have failed.")
    }
}
