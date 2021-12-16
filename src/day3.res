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

// returns tuple of (zero count, one count)
let countBits = (inputs, n) => {
  List.reduce(inputs, (0, 0), ((zero, one), bin) => {
    if Js.String.charAt(n, bin) == "1" {
      (zero, one + 1)
    } else {
      (zero + 1, one)
    }
  })
}

let part1 = inputs => {
  let bits = List.headExn(inputs)->Js.String.length
  let counts = List.makeBy(bits, i => i)->List.map(i => countBits(inputs, i))
  let gamma =
    List.map(counts, ((zero, one)) => zero > one ? "0" : "1")
    ->List.toArray
    ->Js.String.concatMany("")
    ->binToDec
  let epsilon =
    List.map(counts, ((zero, one)) => zero > one ? "1" : "0")
    ->List.toArray
    ->Js.String.concatMany("")
    ->binToDec
  Js.log2("Part 1:", gamma * epsilon)
}

let part2 = inputs => {
  let bits = List.headExn(inputs)->Js.String.length
  let oxygen =
    List.makeBy(bits, i => i)
    ->List.reduce(inputs, (acc, idx) => {
      let (zero, one) = countBits(acc, idx)
      switch acc {
      | list{_} => acc // stop when one item remains
      | _ => {
          let keep = zero > one ? "0" : "1"
          List.keep(acc, x => Js.String.charAt(idx, x) === keep)
        }
      }
    })
    ->List.headExn
    ->binToDec
  let carbon =
    List.makeBy(bits, i => i)
    ->List.reduce(inputs, (acc, idx) => {
      let (zero, one) = countBits(acc, idx)
      switch acc {
      | list{_} => acc
      | _ => {
          let keep = zero <= one ? "0" : "1"
          List.keep(acc, x => Js.String.charAt(idx, x) === keep)
        }
      }
    })
    ->List.headExn
    ->binToDec
  Js.log2("Part 2:", oxygen * carbon)
}

let data = [
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010",
]

let inputs = List.fromArray(data)

part1(inputs)

part2(inputs)
