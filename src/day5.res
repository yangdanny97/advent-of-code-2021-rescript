open Belt

module CoordCmp = Id.MakeComparable({
  type t = (int, int)
  let cmp = ((x1, y1), (x2, y2)) => {
    let fst = Pervasives.compare(x1, x2)
    fst == 0 ? Pervasives.compare(y1, y2) : fst
  }
})

let part1 = inputs => {
  // build map of coordinate => number of lines passing through
  let map = List.reduce(inputs, Map.make(~id=module(CoordCmp)), (currMap, (x1, y1, x2, y2)) => {
    if x1 == x2 || y1 == y2 {
      let (x1, x2) = (Js.Math.min_int(x1, x2), Js.Math.max_int(x1, x2))
      let (y1, y2) = (Js.Math.min_int(y1, y2), Js.Math.max_int(y1, y2))
      let length = Js.Math.max_int(y2 - y1 + 1, x2 - x1 + 1)
      List.makeBy(length, i => i)->List.reduce(currMap, (currMap, d) => {
        let dx = x1 == x2 ? 0 : d
        let dy = y1 == y2 ? 0 : d
        let val = Map.getWithDefault(currMap, (x1 + dx, y1 + dy), 0)
        Map.set(currMap, (x1 + dx, y1 + dy), val + 1)
      })
    } else {
      // ignore diagonal lines
      currMap
    }
  })
  let intersections = Map.reduce(map, 0, (count, _, v) => {
    if v > 1 {
      count + 1
    } else {
      count
    }
  })
  Js.log("Part 1: " ++ Js.Int.toString(intersections))
}

// helper function to calculate current coord based on start, end, offset
let delta = (start, end, offset) => {
  if start > end {
    start - offset
  } else if start < end {
    start + offset
  } else {
    start
  }
}

let part2 = inputs => {
  // the logic is similar to part 1, updated to handle diagonal lines correctly
  let map = List.reduce(inputs, Map.make(~id=module(CoordCmp)), (currMap, (x1, y1, x2, y2)) => {
    let xdiff = Js.Math.max_int(x1, x2) - Js.Math.min_int(x1, x2)
    let ydiff = Js.Math.max_int(y1, y2) - Js.Math.min_int(y1, y2)
    let length = Js.Math.max_int(xdiff + 1, ydiff + 1)
    List.makeBy(length, i => i)->List.reduce(currMap, (currMap, d) => {
      let x = delta(x1, x2, d)
      let y = delta(y1, y2, d)
      let val = Map.getWithDefault(currMap, (x, y), 0)
      Map.set(currMap, (x, y), val + 1)
    })
  })
  let intersections = Map.reduce(map, 0, (count, _, v) => {
    if v > 1 {
      count + 1
    } else {
      count
    }
  })
  Js.log("Part 2: " ++ Js.Int.toString(intersections))
}

let inputs = [
  (0, 9, 5, 9),
  (8, 0, 0, 8),
  (9, 4, 3, 4),
  (2, 2, 2, 1),
  (7, 0, 7, 4),
  (6, 4, 2, 0),
  (0, 9, 2, 9),
  (3, 4, 1, 4),
  (0, 0, 8, 8),
  (5, 5, 8, 2),
]

part1(List.fromArray(inputs))
part2(List.fromArray(inputs))
