open Belt

type direction =
  | Up
  | Down
  | Forward

let part1 = inputs => {
  let (x, y) = List.reduce(inputs, (0, 0), ((x, y), (dir, amt)) => {
    switch dir {
    | Up => (x, y - amt)
    | Down => (x, y + amt)
    | Forward => (x + amt, y)
    }
  })
  Js.log2("Part 1:", x * y)
}

let part2 = inputs => {
  let (x, y, _) = List.reduce(inputs, (0, 0, 0), ((x, y, aim), (dir, amt)) => {
    switch dir {
    | Up => (x, y, aim - amt)
    | Down => (x, y, aim + amt)
    | Forward => (x + amt, y + aim * amt, aim)
    }
  })
  Js.log2("Part 2:", x * y)
}

let data = [(Forward, 5), (Down, 5), (Forward, 8), (Up, 3), (Down, 8), (Forward, 2)]

let inputs = List.fromArray(data)

part1(inputs)

part2(inputs)
