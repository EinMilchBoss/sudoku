use std::{
    fmt::{self},
    process,
    str::FromStr,
};

use sudoku::*;
use thiserror::Error;

use itertools::{Either, Itertools};

fn main() {
    let input =
        match "108720005002008000000009070009340050000007000060000001900000400004250030000080000"
            .parse::<Grid>()
        {
            Ok(grid) => grid,
            Err(error) => {
                eprintln!("{}", build_parse_error_message(&error));
                process::exit(1);
            }
        };

    match input.solve_grid() {
        Some(solution) => println!("{solution}"),
        None => {
            eprintln!("Could not find a solution.");
            process::exit(1);
        }
    }
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
