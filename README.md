# Advent of Code 2021

This is a gathering of all my solutions to Advent of Code 2021 in Haskell. It is mostly to practice my Haskell a bit. I am usually slow so it should not spoil anything before 16-20 hours after reveal.

I do not care about speed or efficiency. I try to make those solutions at least a little bit elegant but I wont loose sleep over it if they aren't. Only one rule: nothing out of `prelude` and `base` for the problems (I still use `file` in `Main.hs`)

## Building

This program is built with [cabal](https://www.haskell.org/cabal/). You should install it on your system using your usual package manager.

You can then download and build the program as follow:

```bash 
# Use git
$ git clone https://github.com/TechnoTecna/advent-of-code-2021.git

# Move into the directory
$ cd advent-of-code2021

# Build the project with cabal
$ cabal build
```

## Usage

You can now run the program with cabal as follow:

```bash
$ cabal run aoc21 
```

There are a few option:

``` bash
# Check inputs from a different directory
cabal run aoc21 -- -d <dir>

# Check only day 4, 5 and 6
cabal run aoc21 -- 4 5 6

# Check solution 1 with a specific input file
cabal run aco21 -- -f <file> 1
```

## Installation

If you want you can also install the program on your machine with:

```
$ cabal install
```

It should then be available in your `PATH`. Note that you can then pass argument normally without double dash (example: `aoc21 -d <dir> 4 1`).
