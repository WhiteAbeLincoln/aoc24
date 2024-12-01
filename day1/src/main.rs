use itertools::{Either, Itertools};
use std::{
    collections::HashMap,
    io::{self, read_to_string},
};

fn main() -> io::Result<()> {
    let input = read_to_string(io::stdin())?;
    let (mut left, mut right) = str_to_ints(&input);
    part1(&mut left, &mut right);
    part2(&left, &right);
    Ok(())
}

/**
gets the total distance by getting the distance
between each pair of the input list in sorted order
*/
fn part1(left: &mut Vec<i32>, right: &mut Vec<i32>) {
    left.sort();
    right.sort();
    println!("total distance: {}", total_distance(&left, &right));
}

/**
gets the total similarity score by summing the similarity
scores for each item in the left list
*/
fn part2(left: &[i32], right: &[i32]) {
    // turn right into a map of int to count
    let mut m: HashMap<i32, usize> = HashMap::new();
    for v in right {
        *m.entry(*v).or_default() += 1;
    }

    // now for each on the left, multiply by the number of times
    // found on the right, and sum
    let total: usize = left
        .iter()
        .map(|x| (*x as usize) * m.get(x).unwrap_or(&0))
        .sum();

    println!("total similarity: {}", total)
}

fn str_to_ints(input: &str) -> (Vec<i32>, Vec<i32>) {
    input
        .split_ascii_whitespace()
        .map(|s| s.parse::<i32>().unwrap())
        .enumerate()
        .partition_map(|(i, val)| {
            if i % 2 == 0 {
                Either::Left(val)
            } else {
                Either::Right(val)
            }
        })
}

fn total_distance(left: &[i32], right: &[i32]) -> i32 {
    left.iter()
        .zip(right.iter())
        .map(|(a, b)| (a - b).abs())
        .sum()
}
