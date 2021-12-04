# Adven of Code 2021

This is a gathering of all my solutions to Advent of Code 2021

## Building

This package is built with [cabal](https://www.haskell.org/cabal/). You should install it on your system using your usual package manager.

You can then download and build the program as follow:

```bash 
# Use git
$ git clone https://github.com/TechnoTecna/advent-of-code-2021.git

# Move into the directory
$ cd adven-of-code-2021

# Build the project with cabal
$ cabal build
```

## Usage

You can now run the program with stack as follow:

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

It should then be available in your `PATH`.
