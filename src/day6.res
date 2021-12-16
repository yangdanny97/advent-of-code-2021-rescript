open Belt

let inc = (map, k, n) => {
  Map.Int.set(map, k, Map.Int.getWithDefault(map, k, 0.0) +. n)
}

let simulate = (inputs, nDays) => {
  // int -> float map of counter value -> number of fish with that value
  let initMap = Array.reduce(inputs, Map.Int.empty, (acc, x) => {
    inc(acc, x, 1.0)
  })
  let finalMap = List.make(nDays, 0)->List.reduce(initMap, (currMap, _) => {
    // build new map using previous map's values
    Map.Int.reduce(currMap, Map.Int.empty, (acc, k, num) => {
      if k == 0 {
        inc(acc, 6, num)->inc(8, num)
      } else {
        inc(acc, k - 1, num)
      }
    })
  })
  // return total number of fish at end of simulation
  Map.Int.reduce(finalMap, 0.0, (acc, _, v) => acc +. v)
}

let part1 = inputs => {
  let count = simulate(inputs, 80)
  Js.log2("Part 1:", count)
}

let part2 = inputs => {
  let count = simulate(inputs, 256)
  Js.log2("Part 2:", count)
}

let inputs = [3, 4, 3, 1, 2]

part1(inputs)
part2(inputs)
