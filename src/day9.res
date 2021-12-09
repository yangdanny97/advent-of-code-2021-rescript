open Belt

let part1 = inputs => {
  let points = Array.reduceWithIndex(inputs, list{}, (acc, row, y) => {
    Array.reduceWithIndex(row, acc, (acc, height, x) => {
      // Belt makes all array accesses return Options
      // 10 is default for off-map coords
      let l = row[x - 1]->Option.getWithDefault(10)
      let r = row[x + 1]->Option.getWithDefault(10)
      let u = inputs[y - 1]->Option.getWithDefault([])->Array.get(x)->Option.getWithDefault(10)
      let d = inputs[y + 1]->Option.getWithDefault([])->Array.get(x)->Option.getWithDefault(10)
      if l > height && r > height && u > height && d > height {
        list{height, ...acc}
      } else {
        acc
      }
    })
  })
  let risk = List.reduce(points, 0, (acc, x) => acc + x + 1)
  Js.log2("Part 1:", risk)
}

// helper function for printing
let fmt = grid => {
  Array.map(grid, row => {
    Array.map(row, cell => {
      if cell != -1 {
        Int.toString(cell)
      } else {
        "X"
      }
    })->Js.String.concatMany("")
  })->Array.forEach(x => Js.log(x))
  Js.log("")
}

// dfs to find the basin associated with a certain point
// returns the size of the basin associated with the search
// or 0 if the point is not in a basin or is off the map
let rec search = (grid, x, y) => {
  let row = grid[y]->Option.getWithDefault([])
  let height = row[x]->Option.getWithDefault(10)
  if height >= 9 || height < 0 {
    0
  } else {
    // mark grid as visited
    ignore(row[x] = -1)
    1 +
    search(grid, x - 1, y) +
    search(grid, x + 1, y) +
    search(grid, x, y - 1) +
    search(grid, x, y + 1)
  }
}

let part2 = inputs => {
  // mutate the array in-place while performing the search,
  // that way we don't need to allocate more memory
  let basins = Array.reduceWithIndex(inputs, list{}, (acc, row, y) => {
    Array.reduceWithIndex(row, acc, (acc, _, x) => {
      let basin = search(inputs, x, y)
      if basin != 0 {
        list{basin, ...acc}
      } else {
        acc
      }
    })
  })->List.sort((a, b) => b - a)
  switch basins {
  | list{fst, snd, thd, ..._} => Js.log2("Part 2:", fst * snd * thd)
  | _ => ()
  }
}

let inputs = [
  [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
  [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
  [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
  [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
  [9, 8, 9, 9, 9, 6, 5, 6, 7, 8],
]

part1(inputs)
part2(inputs)
