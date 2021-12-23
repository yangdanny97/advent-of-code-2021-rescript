# advent-of-code

[Advent of code 2021](https://adventofcode.com/2021)

Some fun/practice with ReScript. The solutions are mostly in a functional style using immutable data structures where feasible, although mutability is leveraged in some places for performance reasons.

The code file for each day also contains the example input provided on the AOC website, and running the file for a particular day will print the answers to part 1 and part 2 in your console. The entry points for each part are functions named `part1` and `part2`. 

Inputs are mostly represented as array literals because 1) I'm not interested in parsing the raw input text and 2) literals for large lists compile to something nasty, so using an array literal and calling `List.fromArray` makes the most sense.

Setup:
```
npm install
```

To build:
```
npm run re:build
```

To run the code for a particular day (day 1 for example):
```
node src/day1.bs.js
```


