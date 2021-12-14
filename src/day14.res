open Belt

let incr = (map, k) => {
  let count = Map.String.getWithDefault(map, k, 0.0) +. 1.0
  Map.String.set(map, k, count)
}

let incrN = (map, k, n) => {
  let count = Map.String.getWithDefault(map, k, 0.0) +. n
  Map.String.set(map, k, count)
}

// calculate element frequencies
let efreq = template => {
  List.reduce(template, Map.String.empty, incr)
}

// calculate pair frequencies in a template
let pfreq = (template, mapping) => {
  let rec pfHelper = (remaining, acc) => {
    switch remaining {
    | list{c1, c2, ...rest} => {
        let key = c1 ++ c2
        switch Map.String.get(mapping, key) {
        | Some(_) => pfHelper(list{c2, ...rest}, incr(acc, key))
        | None => pfHelper(list{c2, ...rest}, acc)
        }
      }
    | _ => acc
    }
  }
  pfHelper(template, Map.String.empty)
}

let step = (pfreq, efreq, insertions) => {
  // for each step, use pair frequency and element frequency mappings
  // to build new mappings
  Map.String.reduce(pfreq, (pfreq, efreq), ((pairs, elements), pair, count) => {
    switch Map.String.get(insertions, pair) {
    | Some(c) => {
        // if there are N existing XY pairs and they map to insertion Z
        // pair map: decrement XY count by N, increment XZ count by N, increment ZY count by N
        // element map: increment count of element Z by N
        let (fst, snd) = (Js.String.get(pair, 0), Js.String.get(pair, 1))
        let (newp1, newp2) = (fst ++ c, c ++ snd)
        let newElements = incrN(elements, c, count)
        let newPairs = incrN(pairs, pair, -.count)->incrN(newp1, count)->incrN(newp2, count)
        (newPairs, newElements)
      }
    | None => (pairs, elements)
    }
  })
}

let simulate = (input, mapping, steps) => {
  let map = Map.String.fromArray(mapping)
  let template = Js.Array.fromMap(Js.String.castToArrayLike(input), x => x)->List.fromArray
  let (pairs, elements) = (pfreq(template, map), efreq(template))
  let (_, freqs) = List.make(steps, 0)->List.reduce((pairs, elements), ((pairs, elements), _) => {
    step(pairs, elements, map)
  })
  Map.String.reduce(freqs, (min_float, max_float), ((maxf, minf), _, v) => (
    max(maxf, v),
    min(minf, v),
  ))
}

let part1 = (input, mapping) => {
  let (maxf, minf) = simulate(input, mapping, 10)
  Js.log2("Part 1:", maxf -. minf)
}

let part2 = (input, mapping) => {
  let (maxf, minf) = simulate(input, mapping, 40)
  Js.log2("Part 2:", maxf -. minf)
}

let input = "NNCB"

let mapping = [
  ("CH", "B"),
  ("HH", "N"),
  ("CB", "H"),
  ("NH", "C"),
  ("HB", "C"),
  ("HC", "B"),
  ("HN", "C"),
  ("NN", "C"),
  ("BH", "H"),
  ("NC", "B"),
  ("NB", "B"),
  ("BN", "B"),
  ("BB", "N"),
  ("BC", "B"),
  ("CC", "N"),
  ("CN", "C"),
]

part1(input, mapping)
part2(input, mapping)
