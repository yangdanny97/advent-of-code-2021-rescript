open Belt

// based on the analytical solution from
// https://www.reddit.com/r/adventofcode/comments/rom5l5/2021_day_24pen_paper_monad_deparsed/

exception Invariant(string)

let getRelations = inputs => {
  // relations is a list of (i, j, k) where the i-th digit + k = the j-th digit
  let (relations, _) = Array.reduceWithIndex(inputs, (list{}, list{(-1, 0)}), (
    (relations, stack),
    (a, b, c),
    i,
  ) => {
    if a == 1 {
      (relations, list{(i, c), ...stack})
    } else {
      switch stack {
      | list{(i2, c2), ...rest} => (list{(i2, i, c2 + b), ...relations}, rest)
      | _ => raise(Invariant("empty stack"))
      }
    }
  })
  relations
}

let part1 = inputs => {
  let relations = getRelations(inputs)
  let digits = Array.make(14, 0)
  List.forEach(relations, ((i, j, k)) => {
    if k > 0 {
      Array.setExn(digits, j, 9)
      Array.setExn(digits, i, 9 - k)
    } else {
      Array.setExn(digits, i, 9)
      Array.setExn(digits, j, 9 + k)
    }
  })
  Js.log2("Part 1", Array.map(digits, d => Int.toString(d))->Js.String.concatMany(""))
}

let part2 = inputs => {
  let relations = getRelations(inputs)
  let digits = Array.make(14, 0)
  List.forEach(relations, ((i, j, k)) => {
    if k > 0 {
      Array.setExn(digits, i, 1)
      Array.setExn(digits, j, 1 + k)
    } else {
      Array.setExn(digits, j, 1)
      Array.setExn(digits, i, 1 - k)
    }
  })
  Js.log2("Part 2", Array.map(digits, d => Int.toString(d))->Js.String.concatMany(""))
}

let inputs = [
  (1, 11, 16),
  (1, 12, 11),
  (1, 13, 12),
  (26, -5, 12),
  (26, -3, 12),
  (1, 14, 2),
  (1, 15, 11),
  (26, -16, 4),
  (1, 14, 12),
  (1, 15, 9),
  (26, -7, 10),
  (26, -11, 11),
  (26, -6, 6),
  (26, -11, 15),
]

part1(inputs)
part2(inputs)
