open Belt

type fold =
  | X(int)
  | Y(int)

// represent points as immutable set of pairs
module PairCmp = Belt.Id.MakeComparable({
  type t = (int, int)

  let cmp = ((a0, a1), (b0, b1)) =>
    switch Pervasives.compare(a0, b0) {
    | 0 => Pervasives.compare(a1, b1)
    | c => c
    }
})

// return new set of points w/ fold applied
let foldX = (points, xFold) => {
  let (left, right) = Set.partition(points, ((x, _)) => x < xFold)
  // mirror points from right to left
  Set.reduce(right, left, (left, (x, y)) => {
    Set.add(left, (xFold - (x - xFold), y))
  })
}

// return new set of points w/ fold applied
let foldY = (points, yFold) => {
  let (up, down) = Set.partition(points, ((_, y)) => y < yFold)
  // mirror points from bottom to top
  Set.reduce(down, up, (up, (x, y)) => {
    Set.add(up, (x, yFold - (y - yFold)))
  })
}

let part1 = (inputs, folds) => {
  let points = Set.fromArray(inputs, ~id=module(PairCmp))
  let firstFold = List.headExn(folds)
  let folded = switch firstFold {
  | X(x) => foldX(points, x)
  | Y(y) => foldY(points, y)
  }
  Js.log2("Part 1:", Set.size(folded))
}

let part2 = (inputs, folds) => {
  let points = Set.fromArray(inputs, ~id=module(PairCmp))
  let folded = List.reduce(folds, points, (points, fold) => {
    switch fold {
    | X(x) => foldX(points, x)
    | Y(y) => foldY(points, y)
    }
  })
  // print out the folded grid
  Js.log("Part 2:")
  let (maxX, maxY) = Set.reduce(folded, (0, 0), ((maxX, maxY), (x, y)) => {
    (max(x, maxX), max(y, maxY))
  })
  for i in 0 to maxY {
    let row = Array.make(maxX + 1, ".")
    Set.forEach(folded, ((x, y)) => {
      if y == i {
        ignore(row[x] = "#")
      }
    })
    Js.log(Js.String.concatMany(row, ""))
  }
}

let inputs = [
(6,10),
(0,14),
(9,10),
(0,3),
(10,4),
(4,11),
(6,0),
(6,12),
(4,1),
(0,13),
(10,12),
(3,4),
(3,0),
(8,4),
(1,10),
(2,14),
(8,10),
(9,0),
]

let folds = [
    Y(7),
    X(5)
]

part1(inputs, folds->List.fromArray)
part2(inputs, folds->List.fromArray)
