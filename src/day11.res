open Belt

let nsteps = 100

// helper function for printing
let fmt = grid => {
  Array.map(grid, row => {
    Array.map(row, cell => {
      Int.toString(cell) ++ ","
    })->Js.String.concatMany("")
  })->Array.forEach(x => Js.log(x))
  Js.log("")
}

// to make sure part 1 doesn't mutate the input
// used for part 2
let clone = inputs => {
  Array.map(inputs, row => {
    Array.map(row, x => x)
  })
}

// check if position is eligible to flash
// if it is, flash and cascade to adjacent nodes
// return number of flashes triggered as a result
let rec search = (grid, x, y, increment) => {
  let row = grid[y]->Option.getWithDefault([])
  let energy = row[x]->Option.getWithDefault(0)
  if energy > 0 {
    if increment {
      ignore(row[x] = energy + 1)
    }
    if energy > 9 || (increment && energy > 8) {
      // set energy to 0, increment & search adjacent
      ignore(row[x] = 0)
      search(grid, x - 1, y, true) +
      search(grid, x + 1, y, true) +
      search(grid, x, y - 1, true) +
      search(grid, x, y + 1, true) +
      search(grid, x - 1, y - 1, true) +
      search(grid, x + 1, y + 1, true) +
      search(grid, x + 1, y - 1, true) +
      search(grid, x - 1, y + 1, true) + 1
    } else {
      0
    }
  } else {
    0
  }
}

// simulate 1 step - return number of flashes
let simulate = inputs => {
  // mutate the array in-place while performing the simulation
  // that way we don't need to allocate more memory
  Array.forEach(inputs, row => {
    Array.forEachWithIndex(row, (x, val) => {
      ignore(row[x] = val + 1)
    })
  })
  Array.reduceWithIndex(inputs, 0, (acc, row, y) => {
    Array.reduceWithIndex(row, acc, (acc, _, x) => {
      acc + search(inputs, x, y, false)
    })
  })
}

let part1 = inputs => {
  let flashes = List.makeBy(nsteps, x => x)->List.reduce(0, (acc, _) => {
    let nflashes = acc + simulate(inputs)

    // fmt(inputs)
    nflashes
  })
  Js.log2("Part 1:", flashes)
}

let part2 = inputs => {
  let break = ref(false)
  let count = ref(0)
  while !break.contents {
    let nFlashes = simulate(inputs)
    count := count.contents + 1
    if nFlashes == 100 {
      // fmt(inputs)
      Js.log2("Part 2:", count.contents)
      break := true
    }
  }
}

let inputs = [
  [5, 4, 8, 3, 1, 4, 3, 2, 2, 3],
  [2, 7, 4, 5, 8, 5, 4, 7, 1, 1],
  [5, 2, 6, 4, 5, 5, 6, 1, 7, 3],
  [6, 1, 4, 1, 3, 3, 6, 1, 4, 6],
  [6, 3, 5, 7, 3, 8, 5, 4, 7, 8],
  [4, 1, 6, 7, 5, 2, 4, 6, 4, 5],
  [2, 1, 7, 6, 8, 4, 1, 7, 2, 1],
  [6, 8, 8, 2, 8, 8, 1, 1, 3, 4],
  [4, 8, 4, 6, 8, 4, 8, 5, 5, 4],
  [5, 2, 8, 3, 7, 5, 1, 5, 2, 6],
]

part1(clone(inputs))
part2(clone(inputs))
