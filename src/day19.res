open Belt

module Coord = {
  type t = (int, int, int)
  let cmp = ((x1, y1, z1), (x2, y2, z2)) => {
    let fst = Pervasives.compare(x1, x2)
    let snd = Pervasives.compare(y1, y2)
    let thd = Pervasives.compare(z1, z2)
    fst != 0 ? fst : snd != 0 ? snd : thd
  }
}
module CoordCmp = Id.MakeComparable(Coord)

exception Invariant(string)

// create unique identifier for a pair of points
// euclidean distance has collisions with this dataset :/
let pair = ((x1, y1, z1), (x2, y2, z2)) => {
  let (dx, dy, dz) = (abs(x1 - x2), abs(y1 - y2), abs(z1 - z2))
  dx * dx * dx + dy * dy * dy + dz * dz * dz + dx + dy + dz
}

let orderPair = (p1, p2) => {
  Coord.cmp(p1, p2) > 0 ? (p1, p2) : (p2, p1)
}

let orientations = [
  ((x, y, z)) => (x, y, z),
  ((x, y, z)) => (x, z, -y),
  ((x, y, z)) => (x, -y, -z),
  ((x, y, z)) => (x, -z, y),
  ((x, y, z)) => (-x, -y, z),
  ((x, y, z)) => (-x, -z, -y),
  ((x, y, z)) => (-x, y, -z),
  ((x, y, z)) => (-x, z, y),
  ((x, y, z)) => (y, -x, z),
  ((x, y, z)) => (y, z, x),
  ((x, y, z)) => (y, x, -z),
  ((x, y, z)) => (y, -z, -x),
  ((x, y, z)) => (-y, x, z),
  ((x, y, z)) => (-y, z, -x),
  ((x, y, z)) => (-y, -x, -z),
  ((x, y, z)) => (-y, -z, x),
  ((x, y, z)) => (z, y, -x),
  ((x, y, z)) => (z, -x, -y),
  ((x, y, z)) => (z, x, y),
  ((x, y, z)) => (z, -y, x),
  ((x, y, z)) => (-z, -x, y),
  ((x, y, z)) => (-z, y, x),
  ((x, y, z)) => (-z, x, -y),
  ((x, y, z)) => (-z, -y, -x),
]

let getDiff = ((x1, y1, z1), (x2, y2, z2)) => {
  (x1 - x2, y1 - y2, z1 - z2)
}

// return mapping function
let findRotation = (zeroPts, scannerPts) => {
  if (
    Array.size(zeroPts) < 12 ||
    Array.size(scannerPts) < 12 ||
    Array.size(zeroPts) != Array.size(scannerPts)
  ) {
    raise(Invariant("matching points are incorrect"))
  }
  let sort = Js.Array.sortInPlaceWith(Coord.cmp)
  let zeroPts = sort(zeroPts)
  Array.reduce(orientations, None, (acc, orientationFn) => {
    switch acc {
    | Some(_) => acc
    | None => {
        let transformed = Array.map(scannerPts, pt => orientationFn(pt))->sort
        let (c1, c2) = (Array.getExn(zeroPts, 0), Array.getExn(transformed, 0))
        let (dx, dy, dz) as diff = getDiff(c1, c2)
        let isValid = Array.zip(zeroPts, transformed)->Array.reduce(true, (acc, (c1, c2)) => {
          if acc {
            getDiff(c1, c2) == diff
          } else {
            false
          }
        })
        if isValid {
          Some(
            c => {
              let (x, y, z) = orientationFn(c)
              (x + dx, y + dy, z + dz)
            },
          )
        } else {
          None
        }
      }
    }
  })
}

// update zero scanner to have the full map, and return an array of scanner positions
let reduceScanners = (zero, scanners) => {
  let positions = []
  // using a mutable map for performance
  // when a scanner from [scanners] is transformed into the zero scanner's coordinate system
  // it is removed from [scanners] and added to [reference]
  let reference = HashMap.Int.make(~hintSize=50)
  HashMap.Int.set(reference, 0, HashMap.Int.copy(zero))
  while HashMap.Int.size(scanners) > 0 {
    HashMap.Int.keysToArray(scanners)->Array.forEach(idx => {
      let _ = HashMap.Int.keysToArray(reference)->Array.reduce(true, (continue, refIdx) => {
        if !continue {
          continue
        } else {
          let refMap = HashMap.Int.get(reference, refIdx)->Option.getExn
          let refDist = HashMap.Int.keysToArray(refMap)->Set.Int.fromArray
          let scanner = HashMap.Int.get(scanners, idx)->Option.getExn
          let scDist = HashMap.Int.keysToArray(scanner)->Set.Int.fromArray
          let intersection = Set.Int.intersect(scDist, refDist)
          // 12 points should have 66 pairwise distances
          if intersection->Set.Int.size >= 66 {
            let (refPts, scannerPts) = Set.Int.reduce(
              intersection,
              (Set.make(~id=module(CoordCmp)), Set.make(~id=module(CoordCmp))),
              ((z, sc), dist) => {
                let (p1, p2) = HashMap.Int.get(refMap, dist)->Option.getExn
                let (p3, p4) = HashMap.Int.get(scanner, dist)->Option.getExn
                (Set.add(z, p1)->Set.add(p2), Set.add(sc, p3)->Set.add(p4))
              },
            )
            let mapFn = findRotation(Set.toArray(refPts), Set.toArray(scannerPts))
            switch mapFn {
            | Some(mapFn) => {
                let newMap = HashMap.Int.make(~hintSize=1000)
                HashMap.Int.forEach(scanner, (k, (p1, p2)) => {
                  let mapped = orderPair(mapFn(p1), mapFn(p2))
                  if HashMap.Int.get(zero, k)->Option.getWithDefault(mapped) != mapped {
                    raise(Invariant("distance metric collision"))
                  }
                  HashMap.Int.set(zero, k, mapped)
                  HashMap.Int.set(newMap, k, mapped)
                })
                HashMap.Int.remove(scanners, idx)
                HashMap.Int.set(reference, idx, newMap)
                ignore(Js.Array2.push(positions, mapFn((0, 0, 0))))
                false
              }
            | None => true
            }
          } else {
            true
          }
        }
      })
    })
  }
  positions
}

let part1 = inputs => {
  let scanners = HashMap.Int.make(~hintSize=50)
  Array.forEachWithIndex(inputs, (idx, scanner) => {
    let points = HashMap.Int.make(~hintSize=1000)
    Array.forEach(scanner, ((x1, y1, z1) as point1) => {
      Array.forEach(scanner, ((x2, y2, z2) as point2) => {
        if !(x1 == x2 && y1 == y2 && z1 == z2) {
          HashMap.Int.set(points, pair(point1, point2), orderPair(point1, point2))
        }
      })
    })
    HashMap.Int.set(scanners, idx, points)
  })
  let zero = HashMap.Int.get(scanners, 0)->Option.getExn
  HashMap.Int.remove(scanners, 0)
  ignore(reduceScanners(zero, scanners))
  let pts = HashMap.Int.reduce(zero, Set.make(~id=module(CoordCmp)), (acc, _, (p1, p2)) => {
    Set.add(acc, p1)->Set.add(p2)
  })
  Js.log2("Part 1:", Set.size(pts))
}

let part2 = inputs => {
  let scanners = HashMap.Int.make(~hintSize=50)
  Array.forEachWithIndex(inputs, (idx, scanner) => {
    let points = HashMap.Int.make(~hintSize=1000)
    Array.forEach(scanner, ((x1, y1, z1) as point1) => {
      Array.forEach(scanner, ((x2, y2, z2) as point2) => {
        if !(x1 == x2 && y1 == y2 && z1 == z2) {
          HashMap.Int.set(points, pair(point1, point2), orderPair(point1, point2))
        }
      })
    })
    HashMap.Int.set(scanners, idx, points)
  })
  let zero = HashMap.Int.get(scanners, 0)->Option.getExn
  HashMap.Int.remove(scanners, 0)
  let positions = reduceScanners(zero, scanners)
  let max_dist = Array.reduce(positions, 0, (acc, (x1, y1, z1)) => {
    Array.reduce(positions, acc, (acc, (x2, y2, z2)) => {
      max(abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2), acc)
    })
  })
  Js.log2("Part 2:", max_dist)
}

let inputs = [
  [
    (404, -588, -901),
    (528, -643, 409),
    (-838, 591, 734),
    (390, -675, -793),
    (-537, -823, -458),
    (-485, -357, 347),
    (-345, -311, 381),
    (-661, -816, -575),
    (-876, 649, 763),
    (-618, -824, -621),
    (553, 345, -567),
    (474, 580, 667),
    (-447, -329, 318),
    (-584, 868, -557),
    (544, -627, -890),
    (564, 392, -477),
    (455, 729, 728),
    (-892, 524, 684),
    (-689, 845, -530),
    (423, -701, 434),
    (7, -33, -71),
    (630, 319, -379),
    (443, 580, 662),
    (-789, 900, -551),
    (459, -707, 401),
  ],
  [
    (686, 422, 578),
    (605, 423, 415),
    (515, 917, -361),
    (-336, 658, 858),
    (95, 138, 22),
    (-476, 619, 847),
    (-340, -569, -846),
    (567, -361, 727),
    (-460, 603, -452),
    (669, -402, 600),
    (729, 430, 532),
    (-500, -761, 534),
    (-322, 571, 750),
    (-466, -666, -811),
    (-429, -592, 574),
    (-355, 545, -477),
    (703, -491, -529),
    (-328, -685, 520),
    (413, 935, -424),
    (-391, 539, -444),
    (586, -435, 557),
    (-364, -763, -893),
    (807, -499, -711),
    (755, -354, -619),
    (553, 889, -390),
  ],
  [
    (649, 640, 665),
    (682, -795, 504),
    (-784, 533, -524),
    (-644, 584, -595),
    (-588, -843, 648),
    (-30, 6, 44),
    (-674, 560, 763),
    (500, 723, -460),
    (609, 671, -379),
    (-555, -800, 653),
    (-675, -892, -343),
    (697, -426, -610),
    (578, 704, 681),
    (493, 664, -388),
    (-671, -858, 530),
    (-667, 343, 800),
    (571, -461, -707),
    (-138, -166, 112),
    (-889, 563, -600),
    (646, -828, 498),
    (640, 759, 510),
    (-630, 509, 768),
    (-681, -892, -333),
    (673, -379, -804),
    (-742, -814, -386),
    (577, -820, 562),
  ],
  [
    (-589, 542, 597),
    (605, -692, 669),
    (-500, 565, -823),
    (-660, 373, 557),
    (-458, -679, -417),
    (-488, 449, 543),
    (-626, 468, -788),
    (338, -750, -386),
    (528, -832, -391),
    (562, -778, 733),
    (-938, -730, 414),
    (543, 643, -506),
    (-524, 371, -870),
    (407, 773, 750),
    (-104, 29, 83),
    (378, -903, -323),
    (-778, -728, 485),
    (426, 699, 580),
    (-438, -605, -362),
    (-469, -447, -387),
    (509, 732, 623),
    (647, 635, -688),
    (-868, -804, 481),
    (614, -800, 639),
    (595, 780, -596),
  ],
  [
    (727, 592, 562),
    (-293, -554, 779),
    (441, 611, -461),
    (-714, 465, -776),
    (-743, 427, -804),
    (-660, -479, -426),
    (832, -632, 460),
    (927, -485, -438),
    (408, 393, -506),
    (466, 436, -512),
    (110, 16, 151),
    (-258, -428, 682),
    (-393, 719, 612),
    (-211, -452, 876),
    (808, -476, -593),
    (-575, 615, 604),
    (-485, 667, 467),
    (-680, 325, -822),
    (-627, -443, -432),
    (872, -547, -609),
    (833, 512, 582),
    (807, 604, 487),
    (839, -516, 451),
    (891, -625, 532),
    (-652, -548, -490),
    (30, -46, -14),
  ],
]

part1(inputs)
part2(inputs)
