// get input (command line)
// - string of numbers
// - each number is a tile
// parse input into grid
// solve grid (backtracing)

use std::{
    fmt::{self},
    process,
    str::FromStr,
};

use thiserror::Error;

use itertools::{Either, Itertools};

fn main() {
    let input =
        match "210080300060070084030500209000105408000000000402706000301007040720040060004010003"
            .parse::<Grid>()
        {
            Ok(grid) => grid,
            Err(error) => {
                eprintln!("{}", build_parse_error_message(&error));
                process::exit(1);
            }
        };

    println!("{}", input);
}

fn build_parse_error_message(error: &ParseGridError) -> String {
    match error {
        ParseGridError::InvalidChars(invalid_chars) => {
            format!("{}\n{}", error, write_bullet_points(invalid_chars))
        }
        ParseGridError::InvalidLength(_) => error.to_string(),
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
pub enum ParseGridError {
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
    type Err = ParseGridError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let length = s.chars().count();
        if length != NUMBER_OF_TILES {
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

const VALUES_PER_SIDE: usize = 9;
const VALUES_PER_BLOCK_SIDE: usize = 3;

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(values) = self;
        let content = values
            .chunks(VALUES_PER_BLOCK_SIDE * VALUES_PER_SIDE)
            .map(|layer| {
                layer
                    .chunks(VALUES_PER_SIDE)
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
        .collect::<Vec<u8>>()
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
            ParseGridError::InvalidChars(invalid_chars) => {
                assert_eq!(9, invalid_chars.len());
            }
            ParseGridError::InvalidLength(_) => panic!("Expected an `InvalidChars` error."),
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
            ParseGridError::InvalidLength(actual_length) => {
                assert_eq!(input_length, actual_length);
            }
            ParseGridError::InvalidChars(_) => panic!("Expected an `InvalidLength` error."),
        }
    }

    #[test]
    fn from_str_test_too_small() {
        let input_length = NUMBER_OF_TILES - 1;
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
