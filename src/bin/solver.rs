// get input (command line)
// - string of numbers
// - each number is a tile
// parse input into grid
// solve grid (backtracing)

use std::{cmp::Ordering, fmt::Display, str::FromStr};

use itertools::{Either, Itertools};

fn main() {
    let input = "210080300060070084030500209000105408000000000402706000301007040720040060004010003"
        .parse::<Grid>();
    if let Err(error) = input {
        let bullet_points = error
            .iter()
            .map(|message| String::from("- ") + message)
            .join("\n");
        println!("Parsing error:\n{}", bullet_points);
        return;
    }
}

const NUMBER_OF_TILES: usize = 9 * 9;

#[derive(Debug, PartialEq, Eq)]
pub struct Grid(pub [u8; NUMBER_OF_TILES]);

impl FromStr for Grid {
    type Err = Vec<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (values, errors): (Vec<_>, Vec<_>) = s
            .chars()
            .enumerate()
            .map(|(index, char)| {
                char.to_digit(10)
                    .map(|value| value as u8)
                    .ok_or((index, char))
            })
            .partition_map(|result| match result {
                Ok(value) => Either::Left(value),
                Err(error) => Either::Right(error),
            });

        if !errors.is_empty() {
            return Err(errors
                .iter()
                .map(|(index, invalid_char)| {
                    format!("Invalid char '{}' at index {}.", invalid_char, index)
                })
                .collect::<Vec<_>>());
        }

        if values.len() != NUMBER_OF_TILES {
            return Err(vec![format!(
                "String is of length {} instead of {}",
                values.len(),
                NUMBER_OF_TILES
            )]);
        }

        Ok(Grid(values.try_into().unwrap()))
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

        let errors = extract_errors(input_valid_chars.parse::<Grid>());

        assert_eq!(9, errors.len());
    }

    #[test]
    fn from_str_test_right_amount() {
        let input_empty_grid: String = "0".repeat(NUMBER_OF_TILES);

        let grid = input_empty_grid.parse();

        assert_eq!(Ok(Grid([0; NUMBER_OF_TILES])), grid);
    }

    #[test]
    fn from_str_test_not_right_amount() {
        let input_smaller: String = "0".repeat(NUMBER_OF_TILES - 1);
        let input_bigger: String = "0".repeat(NUMBER_OF_TILES + 1);

        let expected_smaller = extract_errors(input_smaller.parse::<Grid>());
        let expected_bigger = extract_errors(input_bigger.parse::<Grid>());

        assert_eq!(1, expected_smaller.len());
        assert_eq!(1, expected_bigger.len());
    }

    fn extract_errors(parsed: Result<Grid, Vec<String>>) -> Vec<String> {
        parsed.expect_err("Parsing should have failed.")
    }
}
