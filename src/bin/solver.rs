use std::process;

use clap::Parser;
use itertools::Itertools;
use sudoku::*;

#[derive(Parser)]
#[command(name = "Sudoku Solver")]
#[command(author = "EinMilchBoss")]
#[command(version)]
#[command(
    about = "A simple sudoku solver.",
    long_about = "A simple sudoku solver that searches for all possible solutions."
)]
struct Cli {
    /// The grid to be solved
    grid: String,
    /// Print the solutions as actual grids
    #[arg(short, long)]
    pretty_print: bool,
}

fn main() {
    let cli = Cli::parse();

    let input =
        match "295743861431865900876192543387459216612387495549216738763534189928671354154938600"
            .parse::<Grid>()
        {
            Ok(grid) => grid,
            Err(error) => {
                eprintln!(
                    "Could not parse input to a valid sudoku grid.\n{error_message}",
                    error_message = build_parse_error_message(&error)
                );
                process::exit(1);
            }
        };

    let solutions = input.solve();

    if solutions.is_empty() {
        println!("Could not find a solution.");
        process::exit(0);
    }

    solutions
        .iter()
        .enumerate()
        .for_each(|(i, solution)| println!("Solution {number}:\n{solution}", number = i + 1));
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
