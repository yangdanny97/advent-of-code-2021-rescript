open Belt

let roll = dice => {
  let val = dice.contents
  if val == 100 {
    dice := 1
  } else {
    dice := val + 1
  }
  val
}

let part1 = (p1, p2) => {
  // brute force simulation
  // since we know this will take thousands of iterations
  // I will not use recursion to avoid SO
  let (pos1, pos2) = (ref(p1), ref(p2))
  let (score1, score2) = (ref(0), ref(0))
  let (dice, rolls) = (ref(1), ref(0))
  let turn1 = ref(true)
  while score1.contents < 1000 && score2.contents < 1000 {
    let moves = roll(dice) + roll(dice) + roll(dice)
    let score = turn1.contents ? score1 : score2
    let pos = turn1.contents ? pos1 : pos2
    let pts = mod(pos.contents + moves, 10)
    pos := (pts == 0 ? 10 : pts)
    score := score.contents + (pts == 0 ? 10 : pts)
    turn1 := !turn1.contents
    rolls := rolls.contents + 3
  }
  Js.log2("Part 1:", rolls.contents * min(score1.contents, score2.contents))
}

module StateCmp = Id.MakeComparable({
  // p1 pos, p1 score, p2 pos, p2 score
  type t = (int, int, int, int)
  let cmp = (a, b) => Pervasives.compare(a, b)
})

// dice sum, number of results in 27
let odds = [(3, 1.0), (4, 3.0), (5, 6.0), (6, 7.0), (7, 6.0), (8, 3.0), (9, 1.0)]

let part2 = (p1, p2) => {
  let (p1win, p2win) = (ref(0.0), ref(0.0))
  // current turn number
  let turn = ref(0)
  // keep track of game state => number of universes
  let init = Map.make(~id=module(StateCmp))->Map.set((p1, 0, p2, 0), 1.0)
  let states = ref(init)
  // stop when there are no remaining un-won states
  while Map.size(states.contents) > 0 {
    turn := turn.contents + 1
    let newMap = Map.reduce(states.contents, Map.make(~id=module(StateCmp)), (
      acc,
      (p1p, p1s, p2p, p2s),
      currCount,
    ) => {
      Array.reduce(odds, acc, (acc, (val, count)) => {
        let (_, p1s', _, p2s') as newState = if mod(turn.contents, 2) == 1 {
          let p = mod(p1p + val, 10)
          let newPos = p == 0 ? 10 : p
          (newPos, p1s + newPos, p2p, p2s)
        } else {
          let p = mod(p2p + val, 10)
          let newPos = p == 0 ? 10 : p
          (p1p, p1s, newPos, p2s + newPos)
        }
        // omit winning states from next iteration
        if p1s' >= 21 {
          p1win := p1win.contents +. currCount *. count
          acc
        } else if p2s' >= 21 {
          p2win := p2win.contents +. currCount *. count
          acc
        } else {
          Map.set(acc, newState, Map.getWithDefault(acc, newState, 0.0) +. currCount *. count)
        }
      })
    })
    states := newMap
  }
  Js.log2("Part 2:", max(p1win.contents, p2win.contents))
}

part1(4, 8)
part2(4, 8)
