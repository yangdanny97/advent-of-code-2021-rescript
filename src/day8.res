open Belt

exception Invariant(string)

let part1 = inputs => {
  let count = List.reduce(inputs, 0, (acc, (_, n)) => {
    let digits = Js.String.split(" ", n)->List.fromArray->List.map(d => Js.String.length(d))
    acc + List.reduce(digits, 0, (acc, l) => l == 2 || l == 3 || l == 4 || l == 7 ? acc + 1 : acc)
  })
  Js.log2("Part 1:", count)
}

// convert string input into a list of sets cont. the characters in a digit
let digitsToSets = nums => {
  Js.String.split(" ", nums)
  ->List.fromArray
  ->List.map(d => Js.String.castToArrayLike(d)->Js.Array.fromMap(x => x)->Set.String.fromArray)
}

// return association list of character-set -> digit
// based on ten digits in first part of input
let createMapping = input => {
  let digits = digitsToSets(input)
  // do the special mappings first
  let (partialMap, rem) = List.reduce(digits, (list{}, list{}), ((assoc, rem), digit) => {
    if Set.String.size(digit) == 2 {
      (list{(1, digit), ...assoc}, rem)
    } else if Set.String.size(digit) == 3 {
      (list{(7, digit), ...assoc}, rem)
    } else if Set.String.size(digit) == 4 {
      (list{(4, digit), ...assoc}, rem)
    } else if Set.String.size(digit) == 7 {
      (list{(8, digit), ...assoc}, rem)
    } else {
      (assoc, list{digit, ...rem})
    }
  })
  let finalMap = List.reduce(rem, partialMap, (assoc, digit) => {
    // check number of intersecting segments with 1 and 4
    let oneX =
      List.getAssoc(assoc, 1, (a, b) => a == b)
      ->Option.getExn
      ->Set.String.intersect(digit)
      ->Set.String.size
    let fourX =
      List.getAssoc(assoc, 4, (a, b) => a == b)
      ->Option.getExn
      ->Set.String.intersect(digit)
      ->Set.String.size
    let size = Set.String.size(digit)
    if oneX == 2 {
      // 0, 3, 9
      if size == 5 {
        list{(3, digit), ...assoc}
      } else if size == 6 && fourX == 4 {
        list{(9, digit), ...assoc}
      } else if size == 6 && fourX == 3 {
        list{(0, digit), ...assoc}
      } else {
        raise(Invariant("expected 0, 3, 9"))
      }
    } else if oneX == 1 {
      if size == 6 {
        list{(6, digit), ...assoc}
      } else if size == 5 && fourX == 2 {
        list{(2, digit), ...assoc}
      } else if size == 5 && fourX == 3 {
        list{(5, digit), ...assoc}
      } else {
        raise(Invariant("expected 2, 5, 6"))
      }
    } else {
      raise(Invariant("no matched segments with 1"))
    }
  })
  // reverse the digit -> set mapping
  List.map(finalMap, ((k, v)) => (v, k))
}

let part2 = inputs => {
  let sum = List.reduce(inputs, 0, (sum, (all, nums)) => {
    // lookup for set of characters -> integer
    let mapping = createMapping(all)
    // convert second part of input into a 4 digit number
    let num =
      digitsToSets(nums)
      ->List.map(s => List.getAssoc(mapping, s, (a, b) => Set.String.eq(a, b))->Option.getExn)
      ->List.reduce(0, (acc, x) => acc * 10 + x)
    sum + num
  })
  Js.log2("Part 2:", sum)
}

let inputs = [
  ("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb", "fdgacbe cefdb cefbgd gcbe"),
  ("edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec", "fcgedb cgb dgebacf gc"),
  ("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef", "cg cg fdcagb cbg"),
  ("fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega", "efabcd cedba gadfec cb"),
  ("aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga", "gecf egdcabf bgf bfgea"),
  ("fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf", "gebdcfa ecba ca fadegcb"),
  ("dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf", "cefg dcbef fcge gbcadfe"),
  ("bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd", "ed bcgafe cdgba cbgef"),
  ("egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg", "gbdfcae bgc cg cgb"),
  ("gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc", "fgae cfgab fg bagce"),
]

part1(inputs->List.fromArray)
part2(inputs->List.fromArray)
