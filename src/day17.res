open Belt

let rec simulate = (x, y, xvel, yvel, (minX, maxX, minY, maxY) as bounds) => {
  if y < minY || x > maxX {
    false
  } else if y <= maxY && x >= minX {
    true
  } else {
    simulate(x + xvel, y + yvel, max(xvel - 1, 0), yvel - 1, bounds)
  }
}

let part1 = ((_, maxX, minY, _) as bounds) => {
  let bestYvel = List.makeBy(maxX + 1, x => x)->List.reduce(0, (best, xvel) => {
    List.makeBy(Js.Math.abs_int(minY) + 1, y => y)->List.reduce(best, (best, yvel) => {
      if simulate(0, 0, xvel, yvel, bounds) {
        max(best, yvel)
      } else {
        best
      }
    })
  })
  Js.log2("Part 1:", bestYvel * (bestYvel + 1) / 2)
}

let part2 = ((_, maxX, minY, _) as bounds) => {
  let count = List.makeBy(maxX + 1, x => x)->List.reduce(0, (acc, xvel) => {
    List.makeBy(Js.Math.abs_int(minY) + 1, y => y)->List.reduce(acc, (acc, yvel) => {
      let posY = simulate(0, 0, xvel, yvel, bounds) ? 1 : 0
      let negY = if yvel !== 0 {
        simulate(0, 0, xvel, -yvel, bounds) ? 1 : 0
      } else {
        0
      }
      acc + posY + negY
    })
  })
  Js.log2("Part 2:", count)
}

let input = (20, 30, -10, -5)

part1(input)
part2(input)
