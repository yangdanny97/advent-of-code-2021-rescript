open Belt

let part1 = inputs => {
  let (increases, _) = List.reduce(inputs, (0, List.headExn(inputs)), ((inc, last), x) => {
    if x > last {
      (inc + 1, x)
    } else {
      (inc, x)
    }
  })
  Js.log("Part 1: " ++ Int.toString(increases))
}

let part2 = inputs => {
  switch inputs {
  | list{fst, snd, thd, ...tl} => {
      let acc = (fst, snd, thd, 0)
      let (_, _, _, increases) = List.reduce(tl, acc, ((fst, snd, thd, inc), x) => {
        if fst < x {
          (snd, thd, x, inc + 1)
        } else {
          (snd, thd, x, inc)
        }
      })
      Js.log("Part 2: " ++ Int.toString(increases))
    }
  | _ => ()
  }
}

let data = [
    199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263
]

let inputs = List.fromArray(data)

part1(inputs)

part2(inputs)
