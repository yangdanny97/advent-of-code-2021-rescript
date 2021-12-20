open Belt

let binToDec = bin => {
  let bits = Js.String.length(bin)
  let iter = List.makeBy(bits, i => i)
  List.reduce(iter, 0, (acc, i) => {
    if Js.String.charAt(bits - i - 1, bin) === "0" {
      acc
    } else {
      acc + Int.fromFloat(2. ** Int.toFloat(i))
    }
  })
}

let getBin = (input, default, x, y) => {
    let row = Array.get(input, y)->Option.getWithDefault([])
    let val = Array.get(row, x)->Option.getWithDefault(default)
    if val == "." {
        "0"
    } else {
        "1"
    }
}

let enhance = ((input, edges), enhancement) => {
    let (h, w) = (Array.size(input), Array.size(Array.getExn(input, 0)))
    let output = Array.makeBy(h + 2, y => {
        Array.makeBy(w + 2, x => {
            let x = x - 1
            let y = y - 1
            let getBin = getBin(input, edges)
            let bin = getBin(x - 1, y - 1) ++ 
                getBin(x, y - 1) ++
                getBin(x + 1, y - 1) ++
                getBin(x - 1, y ) ++ 
                getBin(x, y ) ++
                getBin(x + 1, y ) ++
                getBin(x - 1, y + 1) ++ 
                getBin(x, y + 1) ++
                getBin(x + 1, y + 1)
            let idx = binToDec(bin)
            Js.String.get(enhancement, idx)
        })
    })
    let default = edges == "." ? 0 : 511
    let edges = Js.String.get(enhancement, default) == "." ? "." : "#"
    (output, edges)
}

let part1 = (input, enhancement) => {
    // using arrays here because we want efficient random access
    let processedInput = Array.map(input, row => {
        Js.Array.fromMap(Js.String.castToArrayLike(row), x => x)
    })
    let (enhanced, _) = enhance((processedInput, "."), enhancement)->enhance(enhancement)
    let count = Array.reduce(enhanced, 0, (acc, row) => {
        Array.reduce(row, acc, (acc, val) => {
            if val == "." {
                acc
            } else {
                acc + 1
            }
        })
    })
    Js.log2("Part 1:", count)
}

let part2 = (input, enhancement) => {
    let processedInput = Array.map(input, row => {
        Js.Array.fromMap(Js.String.castToArrayLike(row), x => x)
    })
    let (enhanced, _) = Array.make(50, ())->Array.reduce((processedInput, "."), (acc, _) => {
        enhance(acc, enhancement)
    })
    let count = Array.reduce(enhanced, 0, (acc, row) => {
        Array.reduce(row, acc, (acc, val) => {
            if val == "." {
                acc
            } else {
                acc + 1
            }
        })
    })
    Js.log2("Part 2:", count)
}

let enhancement = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
let input = [
"#..#.",
"#....",
"##..#",
"..#..",
"..###"
]

part1(input, enhancement)
part2(input, enhancement)