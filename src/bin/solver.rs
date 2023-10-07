// get input (command line)
// - string of numbers
// - each number is a tile
// parse input into grid
// solve grid (backtracing)

use std::{cmp::Ordering, fmt::Display, process, str::FromStr};

use thiserror::Error;

use itertools::{Either, Itertools};

fn main() {
    let input =
        match "210080300060070084030500209000105408000000000402706000301007040720040060004010003"
            .parse::<Grid>()
        {
            Ok(grid) => grid,
            Err(error) => {
                eprintln!("{}", build_parse_error_message(error));
                process::exit(1);
            }
        };
}

fn build_parse_error_message(error: GridParsingError) -> String {
    match &error {
        GridParsingError::InvalidChars(invalid_chars) => {
            format!("{}\n{}", error, write_bullet_points(invalid_chars))
        }
        GridParsingError::InvalidLength(_) => error.to_string(),
    }
}

fn write_bullet_points(invalid_chars: &[InvalidChar]) -> String {
    invalid_chars
        .iter()
        .map(InvalidChar::to_bullet_point)
        .join("\n")
}

const NUMBER_OF_TILES: usize = 9 * 9;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum GridParsingError {
    #[error("One or more chars were invalid.")]
    InvalidChars(Vec<InvalidChar>),
    #[error("Expected an input of size {}, found {} instead.", NUMBER_OF_TILES, .0)]
    InvalidLength(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidChar {
    index: usize,
    invalid_char: char,
}

impl InvalidChar {
    fn to_bullet_point(&self) -> String {
        format!(
            "- Invalid char '{}' at position {}.",
            self.invalid_char, self.index
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Grid(pub [u8; NUMBER_OF_TILES]);

impl FromStr for Grid {
    type Err = GridParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let length = s.chars().count();
        if length != NUMBER_OF_TILES {
            return Err(GridParsingError::InvalidLength(length));
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
            Err(GridParsingError::InvalidChars(errors))
        }
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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
                .collect::<Vec<u8>>()
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
            GridParsingError::InvalidChars(invalid_chars) => {
                assert_eq!(9, invalid_chars.len());
            }
            GridParsingError::InvalidLength(_) => panic!("Expected an `InvalidChars` error."),
        }
    }

    #[test]
    fn from_str_test_right_amount() {
        let input_empty_grid: String = "0".repeat(NUMBER_OF_TILES);

        let grid = input_empty_grid.parse();

        assert_eq!(Ok(Grid([0; NUMBER_OF_TILES])), grid);
    }

    #[test]
    fn from_str_test_too_big() {
        let input_length = NUMBER_OF_TILES + 1;
        let input_bigger: String = "0".repeat(input_length);

        let error = extract_errors(input_bigger.parse::<Grid>());

        match error {
            GridParsingError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            GridParsingError::InvalidChars(_) => panic!("Expected an `InvalidLength` error."),
        }
    }

    #[test]
    fn from_str_test_too_small() {
        let input_length = NUMBER_OF_TILES - 1;
        let input_smaller: String = "0".repeat(input_length);

        let error = extract_errors(input_smaller.parse::<Grid>());

        match error {
            GridParsingError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            GridParsingError::InvalidChars(_) => panic!("Expected an `InvalidLength` error."),
        }
    }

    fn extract_errors(parsed: Result<Grid, GridParsingError>) -> GridParsingError {
        parsed.expect_err("Parsing should have failed.")
    }
}
