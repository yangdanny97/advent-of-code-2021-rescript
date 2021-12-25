open Belt

module CoordCmp = Id.MakeComparable({
  type t = (int, int)
  let cmp = (a, b) => Pervasives.compare(a, b)
})

type dir = E | S

let printMap = (w, h, map) => {
  Js.log("")
  for y in 0 to h - 1 {
    let row = Array.makeBy(w, x => {
      switch Map.get(map, (x, y)) {
      | Some(E) => ">"
      | Some(S) => "v"
      | _ => "."
      }
    })
    Js.log(Js.String.concatMany(row, ""))
  }
}

let part1 = inputs => {
  let h = Array.length(inputs)
  let w = inputs[0]->Option.getExn->String.length
  let init = Array.reduceWithIndex(inputs, Map.make(~id=module(CoordCmp)), (acc, row, y) => {
    Js.Array2.fromMap(Js.String.castToArrayLike(row), x => x)->Array.reduceWithIndex(acc, (
      acc,
      val,
      x,
    ) => {
      if val == ">" {
        Map.set(acc, (x, y), E)
      } else if val == "v" {
        Map.set(acc, (x, y), S)
      } else {
        acc
      }
    })
  })
  let stop = ref(false)
  let state = ref(init)
  let turns = ref(0)
  while !stop.contents {
    // doing this immutable is not performant since the input is not sparse
    turns := turns.contents + 1
    let curr = state.contents
    let eastMoved = Map.reduce(curr, Map.make(~id=module(CoordCmp)), (acc, (x, y), dir) => {
      switch dir {
      | E => {
          let nextX = mod(x + 1, w)
          if Map.has(curr, (nextX, y)) {
            Map.set(acc, (x, y), E)
          } else {
            Map.set(acc, (nextX, y), E)
          }
        }
      | _ => Map.set(acc, (x, y), S)
      }
    })
    let southMoved = Map.reduce(eastMoved, Map.make(~id=module(CoordCmp)), (acc, (x, y), dir) => {
      switch dir {
      | S => {
          let nextY = mod(y + 1, h)
          if Map.has(eastMoved, (x, nextY)) {
            Map.set(acc, (x, y), S)
          } else {
            Map.set(acc, (x, nextY), S)
          }
        }
      | _ => Map.set(acc, (x, y), E)
      }
    })
    if southMoved == curr {
      stop := true
    }
    state := southMoved
  }
  Js.log2("Part 1", turns.contents)
}

let inputs = [
  "v...>>.vv>",
  ".vv>>.vv..",
  ">>.>v>...v",
  ">>v>>.>.v.",
  "v>v.vv.v..",
  ">.>>..v...",
  ".vv..>.>v.",
  "v.v..>>v.v",
  "....v..v.>",
]

part1(inputs)
