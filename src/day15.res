open Belt

module PairHash = Belt.Id.MakeHashable({
  type t = (int, int)
  let hash = ((x, y)) => x * 1000 + y
  let eq = ((x1, y1), (x2, y2)) => x1 == x2 && y1 == y2
})

// linear time, could be improved with a pq that allows priority change
let minDist = (graph, nodes) => {
  HashSet.reduce(nodes, (0, 0, max_int), ((minx, miny, mindist), (x, y)) => {
    let (_, dist) = HashMap.get(graph, (x, y))->Option.getExn
    if dist < mindist {
      (x, y, dist)
    } else {
      (minx, miny, mindist)
    }
  })
}

// dijkstra's without a priority queue
// iterative approach to reduce risk of stack overflows
let search = (graph, nodes) => {
  let size = ref(HashSet.size(nodes))
  while size.contents > 0 {
    let (x, y, dist) = minDist(graph, nodes)
    HashSet.remove(nodes, (x, y))
    size := size.contents - 1
    let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    Array.forEach(neighbors, ((x, y) as n) => {
      if HashSet.has(nodes, (x, y)) {
        let (risk, currDist) = HashMap.get(graph, n)->Option.getExn
        let alt = risk + dist
        if alt < currDist {
          HashMap.set(graph, n, (risk, alt))
        }
      }
    })
  }
}

let part1 = inputs => {
  let size = Array.length(inputs)
  // graph: HashMap of node -> (node risk, min cost path)
  let graph = HashMap.make(~hintSize=10000, ~id=module(PairHash))
  // nodes: set of eligible nodes
  let nodes = HashSet.make(~hintSize=10000, ~id=module(PairHash))
  Array.forEachWithIndex(inputs, (y, row) => {
    Array.forEachWithIndex(row, (x, risk) => {
      HashMap.set(graph, (x, y), (risk, max_int))
      HashSet.add(nodes, (x, y))
    })
  })
  HashMap.set(graph, (0, 0), (0, 0))
  search(graph, nodes)
  let (_, dist) = HashMap.get(graph, (size - 1, size - 1))->Option.getExn
  Js.log2("Part 1:", dist)
}

let part2 = inputs => {
  let size = Array.length(inputs)
  let graph = HashMap.make(~hintSize=250000, ~id=module(PairHash))
  let nodes = HashSet.make(~hintSize=250000, ~id=module(PairHash))
  Array.forEachWithIndex(inputs, (y, row) => {
    Array.forEachWithIndex(row, (x, risk) => {
      for i in 0 to 4 {
        for j in 0 to 4 {
          let n = risk + i + j
          let adjrisk = if n >= 10 {
            mod(n, 10) + 1
          } else {
            n
          }
          let coord = (x + size * i, y + size * j)
          HashMap.set(graph, coord, (adjrisk, max_int))
          HashSet.add(nodes, coord)
        }
      }
    })
  })
  HashMap.set(graph, (0, 0), (0, 0))
  search(graph, nodes)
  let (_, dist) = HashMap.get(graph, (size * 5 - 1, size * 5 - 1))->Option.getExn
  Js.log2("Part 2:", dist)
}

let inputs = [
  [1, 1, 6, 3, 7, 5, 1, 7, 4, 2],
  [1, 3, 8, 1, 3, 7, 3, 6, 7, 2],
  [2, 1, 3, 6, 5, 1, 1, 3, 2, 8],
  [3, 6, 9, 4, 9, 3, 1, 5, 6, 9],
  [7, 4, 6, 3, 4, 1, 7, 1, 1, 1],
  [1, 3, 1, 9, 1, 2, 8, 1, 3, 7],
  [1, 3, 5, 9, 9, 1, 2, 4, 2, 1],
  [3, 1, 2, 5, 4, 2, 1, 6, 3, 9],
  [1, 2, 9, 3, 1, 3, 8, 5, 2, 1],
  [2, 3, 1, 1, 9, 4, 4, 5, 8, 1],
]

part1(inputs)
part2(inputs)
