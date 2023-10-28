use std::{fmt, str::FromStr};

use itertools::{Either, Itertools};
use thiserror::Error;

const TILES_PER_GRID: usize = TILES_PER_GRID_SIDE * TILES_PER_GRID_SIDE;
const TILES_PER_GRID_SIDE: usize = 9;
const TILES_PER_BLOCK_SIDE: usize = 3;
const EMPTY_TILE: u8 = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Grid([u8; TILES_PER_GRID]);

impl Grid {
    pub fn solve(&self) -> Option<Vec<Grid>> {
        let Grid(tiles) = self;

        solve_grid_iter(0, *tiles, Vec::new())
    }
}

fn solve_grid_iter(index: usize, mut tiles: [u8; 81], mut results: Vec<Grid>) -> Option<Vec<Grid>> {
    if index == TILES_PER_GRID {
        results.push(Grid(tiles));
        return Some(results);
    }

    if tiles[index] == EMPTY_TILE {
        for value in 1..=9 {
            if is_value_possible(value, index, &tiles) {
                let next_iter = solve_grid_iter(
                    index + 1,
                    {
                        tiles[index] = value;
                        tiles
                    },
                    results.clone(),
                );
                if let Some(next_results) = next_iter {
                    results = next_results;
                }
            }
        }
        if !results.is_empty() {
            Some(results)
        } else {
            None
        }
    } else {
        solve_grid_iter(index + 1, tiles, results)
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

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(values) = self;
        let content = values
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
            .join("\n------+-------+------\n");
        write!(f, "{}", content)
    }
}

impl FromStr for Grid {
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
    use super::*;

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

#[cfg(test)]
mod grid_tests {
    use rstest::*;

    use super::*;

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

#[cfg(test)]
mod grid_display_trait_tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn to_string_test() {
        let grid = test_util::build_grid([
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [9, 8, 7, 6, 5, 4, 3, 2, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [6, 6, 6, 6, 6, 6, 6, 6, 6],
            [7, 7, 7, 7, 7, 7, 7, 7, 7],
            [8, 8, 8, 8, 8, 8, 8, 8, 8],
            [9, 9, 9, 9, 9, 9, 9, 9, 9],
            [1, 1, 1, 1, 1, 1, 1, 1, 1],
        ]);
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

        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod grid_from_str_trait_tests {
    use pretty_assertions::assert_eq;

    use super::*;

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
