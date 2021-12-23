open Belt

module CoordCmp = Id.MakeComparable({
  type t = (int, int)
  let cmp = (a, b) => Pervasives.compare(a, b)
})

type amph = A | B | C | D

let getCost = amph => {
  switch amph {
  | A => 1
  | B => 10
  | C => 100
  | D => 1000
  }
}

let getDestCol = amph => {
  switch amph {
  | A => 2
  | B => 4
  | C => 6
  | D => 8
  }
}

let move = (map, src, dest, amph) => {
  Map.remove(map, src)->Map.set(dest, amph)
}

let px = [0, 1, 3, 5, 7, 9, 10]

let getLegalMoves = (max_y, map) => {
  let moveCost = ((x1, y1), (x2, y2), amph) => {
    (abs(x2 - x1) + abs(max_y - y1) + abs(max_y - y2)) * getCost(amph)
  }
  let checkPath = (map, (x1, y1), (x2, y2)) => {
    let valid = ref(true)
    for y in y1 + 1 to max_y {
      if Map.has(map, (x1, y)) {
        valid := false
      }
    }
    for x in min(x1, x2) + 1 to max(x1, x2) - 1 {
      if Map.has(map, (x, max_y)) {
        valid := false
      }
    }
    for y in y2 to max_y {
      if Map.has(map, (x2, y)) {
        valid := false
      }
    }
    valid.contents
  }

  let checkPassage = (map, src, amph) => {
    let valid = []
    Array.forEach(px, x => {
      if checkPath(map, src, (x, max_y)) {
        ignore(Js.Array2.push(valid, (src, (x, max_y), amph, moveCost(src, (x, max_y), amph))))
      }
    })
    valid
  }
  let checkCave = (map, src, amph) => {
    let dest = getDestCol(amph)
    if checkPath(map, src, (dest, max_y - 1)) {
      let valid = ref(true)
      let result = ref(None)
      for y in 0 to max_y - 1 {
        if valid.contents && result.contents == None {
          if !Map.has(map, (dest, y)) {
            result := Some((src, (dest, y), amph, moveCost(src, (dest, y), amph)))
          } else if Map.getExn(map, (dest, y)) != amph {
            valid := false
          }
        }
      }
      switch result.contents {
      | Some(x) => [x]
      | None => []
      }
    } else {
      []
    }
  }
  let legalMovesHelper = (map, (x, y) as start, amph) => {
    let dest = getDestCol(amph)
    if dest == x {
      // amphipod is in correct cave
      let belowSame = ref(true)
      for y' in 0 to y {
        if Map.getExn(map, (dest, y')) != amph {
          belowSame := false
        }
      }
      if belowSame.contents {
        // final position valid
        []
      } else {
        // amphipod is above incorrect amphipod
        checkPassage(map, start, amph)
      }
    } else if y == max_y {
      // amphipod is in passage
      checkCave(map, start, amph)
    } else if checkPath(map, start, (x, max_y)) {
      // amphipod is in wrong cave
      Js.Array.concat(checkPassage(map, start, amph), checkCave(map, start, amph))
    } else {
      []
    }
  }
  let moves = []
  Map.forEach(map, (k, v) => {
    ignore(Js.Array2.push(moves, legalMovesHelper(map, k, v)))
  })
  Array.concatMany(moves)
}

let isValid = map => {
  Map.reduce(map, true, (acc, (x, _), amph) => {
    acc && getDestCol(amph) == x
  })
}

let memo = HashMap.String.make(~hintSize=100000)

let rec search = (max_y, map) => {
  let key = Js.Json.stringifyAny(Map.toArray(map))->Option.getExn
  let cost = HashMap.String.get(memo, key)
  switch cost {
  | Some(c) => c
  | _ => {
      let legalMoves = getLegalMoves(max_y, map)
      if Array.length(legalMoves) == 0 {
        isValid(map) ? 0 : max_int
      } else {
        let min_cost = Array.reduce(legalMoves, max_int, (min_cost, (src, dest, amph, cost)) => {
          let min_rem_cost = search(max_y, move(map, src, dest, amph))
          if min_rem_cost == max_int {
            min_cost
          } else if min_cost == max_int {
            cost + min_rem_cost
          } else {
            min(min_cost, cost + min_rem_cost)
          }
        })
        HashMap.String.set(memo, key, min_cost)
        min_cost
      }
    }
  }
}

let part1 = inputs => {
  let map = Array.reduce(inputs, Map.make(~id=module(CoordCmp)), (acc, (amph, x, y)) => {
    Map.set(acc, (x, y), amph)
  })
  Js.log2("Part 1:", search(2, map))
}

let part2 = inputs => {
  let map = Array.reduce(inputs, Map.make(~id=module(CoordCmp)), (acc, (amph, x, y)) => {
    Map.set(acc, (x, y), amph)
  })
  Js.log2("Part 2:", search(4, map))
}

let inputs = [
  (B, 2, 1),
  (A, 2, 0),
  (C, 4, 1),
  (D, 4, 0),
  (B, 6, 1),
  (C, 6, 0),
  (D, 8, 1),
  (A, 8, 0),
]

let inputsPart2 = [
  (B, 2, 3),
  (D, 2, 2),
  (D, 2, 1),
  (A, 2, 0),
  (C, 4, 3),
  (C, 4, 2),
  (B, 4, 1),
  (D, 4, 0),
  (B, 6, 3),
  (B, 6, 2),
  (A, 6, 1),
  (C, 6, 0),
  (D, 8, 3),
  (A, 8, 2),
  (C, 8, 1),
  (A, 8, 0),
]

part1(inputs)
part2(inputsPart2)
