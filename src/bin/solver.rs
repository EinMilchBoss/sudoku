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

    let input = cli.grid.parse::<Grid>().unwrap_or_else(|error| {
        eprintln!(
            "Could not parse input to a valid sudoku grid.\n{error_message}",
            error_message = build_parse_error_message(&error)
        );
        process::exit(1);
    });

    let solutions = input.solve();

    if solutions.is_empty() {
        println!("Could not find a solution.");
        return;
    }

    if cli.pretty_print {
        solutions.iter().enumerate().for_each(|(i, solution)| {
            println!(
                "Solution {number}:\n{pretty_solution}",
                number = i + 1,
                pretty_solution = solution.to_pretty_string()
            )
        });
    } else {
        solutions.iter().for_each(|solution| println!("{solution}"));
    }
}

fn build_parse_error_message(error: &ParseGridError) -> String {
    match error {
        ParseGridError::InvalidChars(invalid_chars) => format!(
            "{error}\n{bullet_points}",
            bullet_points = invalid_char_bullet_points(invalid_chars)
        ),
        ParseGridError::InvalidAreas(invalid_areas) => format!(
            "{error}\n{bullet_points}",
            bullet_points = invalid_area_bullet_points(invalid_areas)
        ),
        ParseGridError::InvalidLength(_) => error.to_string(),
    }
}

fn invalid_char_bullet_points(invalid_chars: &[InvalidChar]) -> String {
    invalid_chars
        .iter()
        .map(|invalid_char| {
            let InvalidChar {
                index,
                invalid_char,
            } = invalid_char;
            format!("- Invalid char '{}' at position {}.", invalid_char, index)
        })
        .join("\n")
}

fn invalid_area_bullet_points(invalid_area: &[InvalidArea]) -> String {
    invalid_area
        .iter()
        .map(|invalid_area| match invalid_area {
            InvalidArea::Row(InvalidTile { index, value }) => {
                format!(
                    "Invalid row due to value '{value}' at row {number}.",
                    number = index + 1
                )
            }
            InvalidArea::Column(InvalidTile { index, value }) => {
                format!(
                    "Invalid column due to value '{value}' at column {number}.",
                    number = index + 1
                )
            }
            InvalidArea::Block(InvalidTile { index, value }) => {
                format!(
                    "Invalid block due to value '{value}' at block {number}.",
                    number = index + 1
                )
            }
        })
        .join("\n")
}
