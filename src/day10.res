open Belt

let getScore1 = char => {
  if char == ")" {
    3.0
  } else if char == "]" {
    57.0
  } else if char == "}" {
    1197.0
  } else if char == ">" {
    25137.0
  } else {
    0.0
  }
}

let getScore2 = char => {
  if char == ")" {
    1.0
  } else if char == "]" {
    2.0
  } else if char == "}" {
    3.0
  } else if char == ">" {
    4.0
  } else {
    0.0
  }
}

let validate = (input, scoreFn) => {
  let inputLst = Js.Array.fromMap(Js.String.castToArrayLike(input), x => x)->List.fromArray
  List.reduce(inputLst, (0.0, list{}), ((score, acc), x) => {
    if score != 0.0 {
      (score, list{})
    } else if (
      // open brackets add entry to stack
      x == "{"
    ) {
      (0.0, list{"}", ...acc})
    } else if x == "(" {
      (0.0, list{")", ...acc})
    } else if x == "[" {
      (0.0, list{"]", ...acc})
    } else if x == "<" {
      (0.0, list{">", ...acc})
    } else {
      // close brackets validate against stack
      switch acc {
      | list{hd, ...tl} => if hd == x {
          (0.0, tl)
        } else {
          (scoreFn(x), list{})
        }
      | _ => (scoreFn(x), list{})
      }
    }
  })
}

let part1 = inputs => {
  let totalScore = List.reduce(inputs, 0.0, (acc, x) => {
    let (score, _) = validate(x, getScore1)
    acc +. score
  })
  Js.log2("Part 1:", totalScore)
}

let part2 = inputs => {
  let scores =
    List.map(inputs, input => {
      // use dummy score fn - we care about the stack
      let (_, stack) = validate(input, _ => 1.0)
      List.reduce(stack, 0.0, (acc, x) => {
        acc *. 5.0 +. getScore2(x)
      })
    })
    ->List.keep(x => x != 0.0) // exclude sequences with errors
    ->List.sort((a, b) => Pervasives.compare(a, b)) // sort the scores
  // print middle score
  Js.log2("Part 2:", List.getExn(scores, List.length(scores) / 2))
}

let inputs = [
  "[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]",
]

part1(inputs->List.fromArray)
part2(inputs->List.fromArray)
