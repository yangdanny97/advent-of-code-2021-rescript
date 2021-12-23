open Belt

module CoordCmp = Id.MakeComparable({
  type t = (int, int)
  let cmp = (a, b) => Pervasives.compare(a, b)
})

type amph = | A | B | C | D

let getCost = amph => {
    switch amph {
        |A => 1
        |B => 10
        |C => 100
        |D => 1000
    }
}

let getDestCol = amph => {
    switch amph {
        |A => 2
        |B => 4
        |C => 6
        |D => 8
    }
}

let moveCost = ((x1, y1), (x2, y2), amph) => {
    (abs(x2 - x1) + abs(2 - y1) + abs(2 - y2)) * getCost(amph)
}

let move = (map, src, dest, amph) => {
    Map.remove(map, src)->Map.set(dest, amph)
}

let checkPath = (map, (x1, y1), (x2, y2)) => {
    let valid = ref(true)
    for y in y1 + 1 to 2 {
        if Map.has(map, (x1, y)) {
            valid := false
        }
    }
    for x in min(x1, x2) + 1 to max(x1, x2) - 1 {
        if Map.has(map, (x, 2)) {
            valid := false
        }
    }
    for y in y2 to 2 {
        if Map.has(map, (x2, y)) {
            valid := false
        }
    }
    valid.contents
}

// assumes that src amphipod can move out of starting cave if it's in one
let checkCave = (map, src, amph) => {
    let dest = getDestCol(amph)
    let validPath = checkPath(map, src, (dest, 1))
    if validPath && !Map.has(map, (dest, 0)) {
        // amphipod can move to bottom of cave
        [(src, (dest, 0), amph, moveCost(src, (dest, 0), amph))]
    } else if validPath && Map.getExn(map, (dest, 0)) == amph {
        // aphipod can move to top of cave
        [(src, (dest, 1), amph, moveCost(src, (dest, 1), amph))]
    } else {
        []
    }
}

let px = [0, 1, 3, 5, 7, 9, 10]
// assumes that src amphipod can move out of starting cave if it's in one
let checkPassage = (map, src, amph) => {
    let valid = []
    Array.forEach(px, x => {
        if checkPath(map, src, (x, 2)) {
            ignore(Js.Array2.push(valid, (src, (x, 2), amph, moveCost(src, (x, 2), amph))))
        }
    })
    valid
}

let getLegalMoves = map => {
    let legalMovesHelper = ((x, y) as start, amph) => {
        let dest = getDestCol(amph)
        if dest == x {
            // amphipod is in correct cave
            if y == 0 || Map.getExn(map, (dest, 0)) == amph {
                // final position valid
                []
            } else {
                // amphipod is above incorrect amphipod
                checkPassage(map, start, amph)
            }
        } else if y == 2 {
            // amphipod is in passage
            checkCave(map, start, amph)
        } else if y == 0 {
            // amphipod is at bottom of wrong cave
            if Map.has(map, (x, 1)) {
                []
            } else {
                let p = checkPassage(map, start, amph)
                Array.length(p) == 0 ? p : Js.Array.concat(checkCave(map, start, amph), p)
            }
        } else {
            // amphipod is at top of wrong cave
            let p = checkPassage(map, start, amph)
            Array.length(p) == 0 ? p : Js.Array.concat(checkCave(map, start, amph), p)
        }
    }
    Map.reduce(map, [], (acc, k, v) => {
        ignore(Js.Array2.push(acc, legalMovesHelper(k, v)))
        acc
    })->Array.concatMany
}

let isValid = map => {
    Map.reduce(map, true, (acc, (x, _), amph) => {
        acc && (getDestCol(amph) == x)
    })
}

let memo = HashMap.String.make(~hintSize=100000)

let rec search = map => {
    let key = Js.Json.stringifyAny(Map.toArray(map))->Option.getExn
    let cost = HashMap.String.get(memo, key)
    switch cost {
        |Some(c) => c
        |_ => {
            let legalMoves = getLegalMoves(map)
            if Array.length(legalMoves) == 0 {
                isValid(map) ? 0 : max_int
            } else {
                let min_cost = Array.reduce(legalMoves, max_int, (min_cost, (src, dest, amph, cost)) => {
                    let min_rem_cost = search(move(map, src, dest, amph))
                    if min_rem_cost == max_int {
                        min_cost
                    } else if min_cost == max_int {
                        cost + min_rem_cost
                    } else {
                        cost + min(min_cost, min_rem_cost)
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
    Js.log2("Part 1:", search(map))
let exp = [
    (A, 8, 0),
    (A, 2, 0),
    (B, 4, 1),
    (B, 4, 0),
    (C, 6, 1),
    (C, 6, 0),
    (D, 8, 1),
    (D, 5, 2),
]->Array.reduce(Map.make(~id=module(CoordCmp)), (acc, (amph, x, y)) => {
        Map.set(acc, (x, y), amph)
    })
    Js.log(HashMap.String.get(memo, Js.Json.stringifyAny(Map.toArray(exp))->Option.getExn))
Js.log(getLegalMoves(exp))
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

let inputs2 = [
    (A, 2, 1),
    (B, 2, 0),
    (D, 4, 1),
    (C, 4, 0),
    (B, 6, 1),
    (A, 6, 0),
    (D, 8, 1),
    (C, 8, 0),
]

part1(inputs)