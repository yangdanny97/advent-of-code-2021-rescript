open Belt

// path tree
type rec path = {
  name: string,
  next: list<path>,
}

// build a path tree of all the possible paths
// visited tracks the small rooms that have been visited
let rec buildPath = (map, visited, room, canRevisit) => {
  let isSmallRoom = Js.String.toLowerCase(room) === room
  let visited = isSmallRoom ? Set.String.add(visited, room) : visited

  let children = if room === "end" {
    list{}
  } else {
    Map.String.getWithDefault(map, room, Set.String.empty)->Set.String.reduce(list{}, (acc, r) => {
      if !Set.String.has(visited, r) {
        list{buildPath(map, visited, r, canRevisit), ...acc}
      } else if canRevisit && r != "start" && r != "end" {
        list{buildPath(map, visited, r, false), ...acc}
      } else {
        acc
      }
    })
  }
  {
    name: room,
    next: children,
  }
}

// count number of leaf nodes with "end"
let rec countPaths = path => {
  if path.name === "end" {
    1
  } else {
    List.reduce(path.next, 0, (acc, x) => acc + countPaths(x))
  }
}

// build map of room -> set of rooms it is connected to
let buildMap = inputs => {
  List.reduce(inputs, Map.String.empty, (acc, (r1, r2)) => {
    let r1Set = Map.String.getWithDefault(acc, r1, Set.String.empty)->Set.String.add(r2)
    let r2Set = Map.String.getWithDefault(acc, r2, Set.String.empty)->Set.String.add(r1)
    Map.String.set(acc, r1, r1Set)->Map.String.set(r2, r2Set)
  })
}

let part1 = inputs => {
  let map = buildMap(inputs)
  let pathTree = buildPath(map, Set.String.empty, "start", false)
  let paths = countPaths(pathTree)
  Js.log2("Part 1:", paths)
}

let part2 = inputs => {
  let map = buildMap(inputs)
  let pathTree = buildPath(map, Set.String.empty, "start", true)
  let paths = countPaths(pathTree)
  Js.log2("Part 2:", paths)
}

let inputs = [
  ("start", "A"),
  ("start", "b"),
  ("A", "c"),
  ("A", "b"),
  ("b", "d"),
  ("A", "end"),
  ("b", "end"),
]

part1(inputs->List.fromArray)
part2(inputs->List.fromArray)
