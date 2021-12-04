open Belt

module IntCmp = Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

type box = {
  row: int,
  col: int,
  marked: bool,
}

type board = {
  // number => info mapping
  nums: Map.Int.t<box>,
  // row/col => num of rem unmarked square
  rows: Map.Int.t<int>,
  cols: Map.Int.t<int>,
}

let createBoard = (input: array<array<int>>) => {
  let numbers = Array.reduceWithIndex(input, Map.Int.empty, (map, row, rowN) => {
    Array.reduceWithIndex(row, map, (map, num, colN) => {
      Map.Int.set(
        map,
        num,
        {
          row: rowN,
          col: colN,
          marked: false,
        },
      )
    })
  })
  {
    nums: numbers,
    rows: Map.Int.fromArray([(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]),
    cols: Map.Int.fromArray([(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]),
  }
}

// mark a number on the board if present
// inputs: board state, number
// outputs: new board state, whether or not the board completed a row/col from this move
let processNum = (board, num) => {
  // find number, mark board as side effect
  let foundNum = Map.Int.get(board.nums, num)
  // O(1) win condition check
  switch foundNum {
  | Some(n) => {
      let rowVal = Map.Int.getExn(board.rows, n.row)
      let colVal = Map.Int.getExn(board.cols, n.col)
      let newBoard = {
        nums: Map.Int.set(board.nums, num, {...n, marked: true}),
        rows: Map.Int.set(board.rows, n.row, rowVal - 1),
        cols: Map.Int.set(board.cols, n.col, colVal - 1),
      }
      (rowVal == 1 || colVal == 1, newBoard)
    }
  | None => (false, board)
  }
}

let calculateScore = (board, num) => {
  Map.Int.reduce(board.nums, 0, (acc, k, v) => {
    if v.marked {
      acc
    } else {
      acc + k
    }
  }) *
  num
}

let part1 = (nums, boards) => {
  // track whether or not any board has won in a ref
  let hasWon = ref(false)
  ignore(
    List.reduce(nums, boards, (currState, num) => {
      if hasWon.contents {
        currState
      } else {
        List.reduce(currState, list{}, (acc, board) => {
          if hasWon.contents {
            list{board, ...acc}
          } else {
            let (won, newBoard) = processNum(board, num)
            if won {
              Js.log("Part 1: " ++ Js.Int.toString(calculateScore(newBoard, num)))
              hasWon := true
            }
            list{newBoard, ...acc}
          }
        })
      }
    }),
  )
}

let part2 = (nums, boards) => {
  // track number of remaining boards in play in a ref
  let rem = ref(List.length(boards))
  ignore(
    // track how many + which boards are still in play
    List.reduce(nums, boards, (boardsLeft, num) => {
      if rem.contents == 0 {
        boardsLeft
      } else {
        List.reduce(boardsLeft, list{}, (acc, board) => {
          if rem.contents == 0 {
            acc
          } else {
            let (won, newBoard) = processNum(board, num)
            if won {
              // remove boards from play as they win
              if rem.contents == 1 {
                Js.log("Part 2: " ++ Js.Int.toString(calculateScore(newBoard, num)))
              }
              rem := rem.contents - 1
              acc
            } else {
                list{newBoard, ...acc}
            }
          }
        })
      }
    }),
  )
}

let nums = [
  7,
  4,
  9,
  5,
  11,
  17,
  23,
  2,
  0,
  14,
  21,
  24,
  10,
  16,
  13,
  6,
  15,
  25,
  12,
  22,
  18,
  20,
  8,
  19,
  3,
  26,
  1,
]

let boards = [
  [
    [22, 13, 17, 11, 0],
    [8, 2, 23, 4, 24],
    [21, 9, 14, 16, 7],
    [6, 10, 3, 18, 5],
    [1, 12, 20, 15, 19],
  ],
  [
    [3, 15, 0, 2, 22],
    [9, 18, 13, 17, 5],
    [19, 8, 7, 25, 23],
    [20, 11, 10, 24, 4],
    [14, 21, 16, 12, 6],
  ],
  [
    [14, 21, 17, 24, 4],
    [10, 16, 15, 9, 19],
    [18, 8, 23, 26, 20],
    [22, 11, 13, 6, 5],
    [2, 0, 12, 3, 7],
  ],
]

part1(List.fromArray(nums), List.fromArray(boards)->List.map(createBoard))
part2(List.fromArray(nums), List.fromArray(boards)->List.map(createBoard))
