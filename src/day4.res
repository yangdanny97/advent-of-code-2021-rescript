type box = {
  num: int,
  row: int,
  col: int,
  marked: bool,
}

// mutable state makes for more readable code here,
// since working with a grid generally involves random array accesses
type board = {
  numbers: array<box>,
  // number of unmarked squares remaining in row/col
  // easy victory condition tracking
  rows: array<int>,
  cols: array<int>,
}

let createBoard = (input: array<array<int>>) => {
  let numbers = []
  for row in 0 to 4 {
    for col in 0 to 4 {
      ignore(
        Js.Array2.push(
          numbers,
          {
            num: input[row][col],
            row: row,
            col: col,
            marked: false,
          },
        ),
      )
    }
  }
  {
    numbers: numbers,
    rows: [5, 5, 5, 5, 5],
    cols: [5, 5, 5, 5, 5],
  }
}

// mark a number on the board if present
// input: board state and number
// output: whether the number caused board to win
// side effect: update board state
let processNum = (board, num) => {
  // find number, mark board as side effect
  let foundNum = Js.Array2.reducei(
    board.numbers,
    (acc, box, i) => {
      switch acc {
      | None if box.num == num => {
          board.numbers[i] = {
            ...board.numbers[i],
            marked: true,
          }
          Some(box)
        }
      | _ => acc
      }
    },
    None,
  )
  // O(1) win condition check
  switch foundNum {
  | Some(n) => {
      board.rows[n.row] = board.rows[n.row] - 1
      board.cols[n.col] = board.cols[n.col] - 1
      board.rows[n.row] == 0 || board.cols[n.col] == 0
    }
  | None => false
  }
}

let calculateScore = (board, num) => {
  Js.Array2.reduce(
    board.numbers,
    (acc, n) => {
      if n.marked {
        acc
      } else {
        acc + n.num
      }
    },
    0,
  ) *
  num
}

let part1 = (nums, boards) => {
  ignore(
    Belt.List.reduce(nums, false, (hasWon, num) => {
      if hasWon {
        true
      } else {
        Belt.List.reduce(boards, hasWon, (hasWon, board) => {
          if hasWon {
            true
          } else if processNum(board, num) {
            Js.log("Part 1: " ++ Js.Int.toString(calculateScore(board, num)))
            true
          } else {
            false
          }
        })
      }
    }),
  )
}

let part2 = (nums, boards) => {
  ignore(
    // track how many + which boards are still in play
    Belt.List.reduce(nums, (Belt.List.length(boards), boards), ((rem, boardsLeft), num) => {
      if rem == 0 {
        (0, boardsLeft)
      } else {
        // remove boards from play as they win
        Belt.List.reduce(boardsLeft, (rem, list{}), ((rem, acc), board) => {
          if rem == 0 {
            (0, acc)
          } else if processNum(board, num) {
            if rem == 1 {
                Js.log("Part 2: " ++ Js.Int.toString(calculateScore(board, num)))
            }
            (rem - 1, acc)
          } else {
            (rem, list{board, ...acc})
          }
        })
      }
    }),
  )
}

let nums = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

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

part1(Belt.List.fromArray(nums), Belt.List.fromArray(boards)->Belt.List.map(createBoard))
part2(Belt.List.fromArray(nums), Belt.List.fromArray(boards)->Belt.List.map(createBoard))
