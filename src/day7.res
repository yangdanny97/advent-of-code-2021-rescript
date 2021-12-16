open Belt

let part1Cost = (start, end) => {
  Js.Math.abs_int(start - end)
}

// sum of arithmetic series
let part2Cost = (start, end) => {
  let diff = Js.Math.abs_int(start - end)
  diff * (diff + 1) / 2
}

// get cost for all crabs to move to some position
// costFn calculates the cost of 1 crab
let getCost = (positions, position, costFn) => {
  Map.Int.reduce(positions, 0, (acc, k, v) => acc + costFn(k, position) * v)
}

// calculate minimum cost for all crabs to move to any position given a cost function
let minCost = (input, costFn) => {
  let nCrabs = List.length(input)
  // build a map to calculate the cost for all
  // crabs at a position simultaneously
  let positions = List.reduce(input, Map.Int.empty, (acc, x) => {
    Map.Int.set(acc, x, Map.Int.getWithDefault(acc, x, 0) + 1)
  })
  let minPos = Map.Int.minKey(positions)->Option.getExn
  let maxPos = Map.Int.maxKey(positions)->Option.getExn
  let range = List.makeBy(maxPos - minPos + 1, i => minPos + i)
  List.reduce(range, costFn(maxPos, minPos) * nCrabs, (acc, x) =>
    Js.Math.min_int(acc, getCost(positions, x, costFn))
  )
}

let part1 = input => {
  let cost = minCost(input, part1Cost)
  Js.log2("Part 1:", cost)
}

let part2 = input => {
  let cost = minCost(input, part2Cost)
  Js.log2("Part 2:", cost)
}

let input = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

part1(input->List.fromArray)
part2(input->List.fromArray)
