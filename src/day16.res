open Belt

exception Invariant(string)

let hex2bin = input => {
  let helper = char => {
    switch char {
    | x if x == "0" => "0000"
    | x if x == "1" => "0001"
    | x if x == "2" => "0010"
    | x if x == "3" => "0011"
    | x if x == "4" => "0100"
    | x if x == "5" => "0101"
    | x if x == "6" => "0110"
    | x if x == "7" => "0111"
    | x if x == "8" => "1000"
    | x if x == "9" => "1001"
    | x if x == "A" => "1010"
    | x if x == "B" => "1011"
    | x if x == "C" => "1100"
    | x if x == "D" => "1101"
    | x if x == "E" => "1110"
    | x if x == "F" => "1111"
    | x => raise(Invariant("invalid hex char " ++ x))
    }
  }
  let mapped = Js.Array2.fromMap(Js.String.castToArrayLike(input), x => helper(x))
  Js.String.concatMany(mapped, "")
}

let binToDec = bin => {
  let bits = Js.String.length(bin)
  let iter = List.makeBy(bits, i => i)
  List.reduce(iter, 0., (acc, i) => {
    if Js.String.charAt(bits - i - 1, bin) === "0" {
      acc
    } else { 
      acc +. (2. ** Int.toFloat(i))
    }
  })
}

// return value and next cursor position
let readTriple = (packet, cursor) => {
  let ver = Js.String.slice(~from=cursor, ~to_=cursor + 3, packet)->binToDec
  (ver, cursor + 3)
}

let readNum = (packet, cursor) => {
  let rec helper = cursor => {
    let bin = Js.String.slice(~from=cursor + 1, ~to_=cursor + 5, packet)
    if Js.String.get(packet, cursor) == "1" {
      let (rest, newCursor) = helper(cursor + 5)
      (bin ++ rest, newCursor)
    } else {
      (bin, cursor + 5)
    }
  }
  let (bin, newCursor) = helper(cursor)
  (bin->binToDec, newCursor)
}

// part 1 - sum up versions

let rec handlePacket = (packet, cursor) => {
  if cursor >= Js.String.length(packet) {
    (0., cursor)
  } else {
    let (version, cursor) = readTriple(packet, cursor)
    let (typeid, cursor) = readTriple(packet, cursor)
    if typeid == 4. {
      let (_, cursor) = readNum(packet, cursor)
      (version, cursor)
    } else {
      let (versionsum, cursor) = handleOperator(packet, cursor)
      (version +. versionsum, cursor)
    }
  }
}
and handleOperator = (packet, cursor) => {
  let (ltid, cursor) = (Js.String.get(packet, cursor), cursor + 1)
  if ltid == "0" {
    let (len, cursor) = (
      Js.String.slice(~from=cursor, ~to_=cursor + 15, packet)->binToDec,
      cursor + 15,
    )
    let rec helper = (target, current, versionsum) => {
      if current >= target {
        (versionsum, current)
      } else {
        let (version, cursor) = handlePacket(packet, current)
        helper(target, cursor, versionsum +. version)
      }
    }
    helper(cursor + Int.fromFloat(len), cursor, 0.)
  } else {
    let (n, cursor) = (
      Js.String.slice(~from=cursor, ~to_=cursor + 11, packet)->binToDec,
      cursor + 11,
    )
    let rec helper = (n, current, versionsum) => {
      if n == 0 {
        (versionsum, current)
      } else {
        let (version, cursor) = handlePacket(packet, current)
        helper(n - 1, cursor, versionsum +. version)
      }
    }
    helper(Int.fromFloat(n), cursor, 0.)
  }
}

// part 2 - calculate value

let rec handlePacket2 = (packet, cursor) => {
  if cursor >= Js.String.length(packet) {
    (0., cursor)
  } else {
    let (_, cursor) = readTriple(packet, cursor)
    let (typeid, cursor) = readTriple(packet, cursor)
    if typeid == 4. {
      let (val, cursor) = readNum(packet, cursor)
      (val, cursor)
    } else {
      let (val, cursor) = handleOperator2(typeid, packet, cursor)
      (val, cursor)
    }
  }
}
and handleOperator2 = (typeid, packet, cursor) => {
  let (ltid, cursor) = (Js.String.get(packet, cursor), cursor + 1)
  let (subpackets, cursor) = if ltid == "0" {
    let (len, cursor) = (
      Js.String.slice(~from=cursor, ~to_=cursor + 15, packet)->binToDec,
      cursor + 15,
    )
    let rec helper = (target, current, acc) => {
      if current >= target {
        (acc, current)
      } else {
        let (val, cursor) = handlePacket2(packet, current)
        helper(target, cursor, list{val, ...acc})
      }
    }
    helper(cursor + Int.fromFloat(len), cursor, list{})
  } else {
    let (n, cursor) = (
      Js.String.slice(~from=cursor, ~to_=cursor + 11, packet)->binToDec,
      cursor + 11,
    )
    let rec helper = (n, current, acc) => {
      if n == 0 {
        (acc, current)
      } else {
        let (val, cursor) = handlePacket2(packet, current)
        helper(n - 1, cursor, list{val, ...acc})
      }
    }
    helper(Int.fromFloat(n), cursor, list{})
  }
  // we built the lists in reverse order
  // order matters for comparators so we fix the order here
  let subpackets = List.reverse(subpackets)
  let val = if typeid == 0. {
      List.reduce(subpackets, 0., (acc, x) => acc +. x)
  } else if typeid == 1. {
      List.reduce(subpackets, 1., (acc, x) => acc *. x)
  } else if typeid == 2. {
      List.reduce(subpackets, List.headExn(subpackets), (acc, x) => min(acc, x))
  } else if typeid == 3. {
      List.reduce(subpackets, List.headExn(subpackets), (acc, x) => max(acc, x))
  } else if typeid == 5. {
      (List.getExn(subpackets, 0) > List.getExn(subpackets, 1)) ? 1. : 0.
  } else if typeid == 6. {
      (List.getExn(subpackets, 0) < List.getExn(subpackets, 1)) ? 1. : 0.
  } else if typeid == 7. {
      (List.getExn(subpackets, 0) == List.getExn(subpackets, 1)) ? 1. : 0.
  } else {
      raise(Invariant("invalid typeid"))
  }
  (val, cursor)
}

let part1 = input => {
  let bin = hex2bin(input)
  let (versionsum, _) = handlePacket(bin, 0)
  Js.log2("Part 1:", versionsum)
}

let part2 = input => {
  let bin = hex2bin(input)
  let (val, _) = handlePacket2(bin, 0)
  Js.log2("Part 2:", val)
}

let input = "D2FE28"

part1(input)
part2(input)
