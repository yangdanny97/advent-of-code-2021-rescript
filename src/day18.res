open Belt

// need mutability here for pointer manipulation
type rec tree =
  | Node(ref<tree>, ref<tree>, ref<option<tree>>)
  | Leaf(ref<int>, ref<option<tree>>)

exception Invariant(string)

let parseInput = input => {
  let rec helper = (input, cursor) => {
    let c = Js.String.get(input, cursor)
    if c === "[" {
      let (left, cursor) = helper(input, cursor + 1)
      let (right, cursor) = helper(input, cursor + 1)
      let node = Node(ref(left), ref(right), ref(None))
      switch left {
      | Node(_, _, p)
      | Leaf(_, p) =>
        p.contents = Some(node)
      }
      switch right {
      | Node(_, _, p)
      | Leaf(_, p) =>
        p.contents = Some(node)
      }
      (node, cursor + 1)
    } else if c === "]" || c === "," {
      raise(Invariant("these should be skipped"))
    } else {
      let val = Int.fromString(c)->Option.getExn
      (Leaf(ref(val), ref(None)), cursor + 1)
    }
  }
  let (tree, _) = helper(input, 0)
  tree
}

let rec printTree = node => {
  switch node {
  | Node(l, r, _) => "[" ++ printTree(l.contents) ++ "," ++ printTree(r.contents) ++ "]"
  | Leaf(val, _) => Int.toString(val.contents)
  }
}

let copy = tree => {
  // dirty but effective way of making a deep copy
  tree->printTree->parseInput
}

// tree traversal utilities

let getParentRef = node => {
  switch node {
  | Node(_, _, p)
  | Leaf(_, p) => p
  }
}

let rec findSuccNum = node => {
  let rec getLnumChild = node => {
    switch node {
    | Node(l, _, _) => getLnumChild(l.contents)
    | Leaf(_, _) => node
    }
  }
  switch getParentRef(node).contents {
  | Some(Node(l, r, _)) if l.contents === node => Some(getLnumChild(r.contents))
  | Some(Node(_, _, _) as n) => findSuccNum(n)
  | _ => None
  }
}

let rec findPredNum = node => {
  let rec getRnumChild = node => {
    switch node {
    | Node(_, r, _) => getRnumChild(r.contents)
    | Leaf(_, _) => node
    }
  }
  switch getParentRef(node).contents {
  | Some(Node(l, r, _)) if r.contents === node => Some(getRnumChild(l.contents))
  | Some(Node(_, _, _) as n) => findPredNum(n)
  | _ => None
  }
}

let updateParent = (parent, original, new) => {
  switch parent {
  | Some(Node(l, r, _)) =>
    if l.contents === original {
      l := new
    } else if r.contents === original {
      r := new
    } else {
      raise(Invariant("pointer is wrong"))
    }
  | None => ()
  | _ => raise(Invariant("parent cannot be leaf"))
  }
}

// tree transformations

let explode = node => {
  switch node {
  | Node(l, r, parent) =>
    switch (l.contents, r.contents) {
    | (Leaf(v1, _), Leaf(v2, _)) => {
        let new = Leaf(ref(0), ref(None))
        getParentRef(new) := parent.contents
        updateParent(parent.contents, node, new)
        switch findPredNum(new) {
        | Some(Leaf(val, _)) => val := val.contents + v1.contents
        | _ => ()
        }
        switch findSuccNum(new) {
        | Some(Leaf(val, _)) => val := val.contents + v2.contents
        | _ => ()
        }
      }
    | _ => raise(Invariant("children must be regular nums"))
    }
  | _ => raise(Invariant("cannot explode leaf"))
  }
}

let split = node => {
  switch node {
  | Leaf(val, parent) => {
      let lval = val.contents / 2
      let rval = mod(val.contents, 2) + lval
      let lc = Leaf(ref(lval), ref(None))
      let rc = Leaf(ref(rval), ref(None))
      let new = Node(ref(lc), ref(rc), ref(parent.contents))
      getParentRef(lc) := Some(new)
      getParentRef(rc) := Some(new)
      updateParent(parent.contents, node, new)
    }
  | _ => raise(Invariant("cannot split pair"))
  }
}

let rec trySplit = num => {
  switch num {
  | Leaf(val, _) =>
    if val.contents >= 10 {
      split(num)
      true
    } else {
      false
    }
  | Node(l, r, _) =>
    if trySplit(l.contents) {
      true
    } else {
      trySplit(r.contents)
    }
  }
}

let rec tryExplode = num => {
  let isNum = node => {
    switch node {
    | Leaf(_, _) => true
    | _ => false
    }
  }
  let rec findPairNesting = node => {
    switch node {
    | Node(_, _, p) =>
      switch p.contents {
      | Some(parent) => 1 + findPairNesting(parent)
      | _ => 1
      }
    | Leaf(_, p) =>
      switch p.contents {
      | Some(parent) => findPairNesting(parent)
      | _ => 0
      }
    }
  }
  switch num {
  | Leaf(_, _) => false
  | Node(l, r, _) =>
    if tryExplode(l.contents) {
      true
    } else if findPairNesting(num) >= 5 && isNum(l.contents) && isNum(r.contents) {
      explode(num)
      true
    } else {
      tryExplode(r.contents)
    }
  }
}

let step = num => {
  if tryExplode(num) {
    true
  } else {
    trySplit(num)
  }
}

let rec reduce = n => {
  if step(n) {
    reduce(n)
  } else {
    n
  }
}

let add = (l, r) => {
  let parent = Node(ref(l), ref(r), ref(None))
  getParentRef(l) := Some(parent)
  getParentRef(r) := Some(parent)
  let res = reduce(parent)
  res
}

let rec magnitude = num => {
  switch num {
  | Leaf(val, _) => val.contents
  | Node(l, r, _) => 3 * magnitude(l.contents) + 2 * magnitude(r.contents)
  }
}

// solution entry point

let part1 = input => {
  let nums = List.fromArray(input)->List.map(i => parseInput(i))
  switch nums {
  | list{hd, ...tl} => {
      let sum = List.reduce(tl, hd, (acc, n) => {
        add(acc, n)
      })
      Js.log(printTree(sum))
      Js.log2("Part 1:", magnitude(sum))
    }
  | _ => ()
  }
}

let part2 = input => {
  let nums = List.fromArray(input)->List.map(i => parseInput(i))
  let rec getMax = (nums, currmax) => {
    switch nums {
    | list{} => currmax
    | list{hd, ...tl} => {
        let newmax = List.reduce(tl, currmax, (acc, x) => {
          let a = add(copy(hd), copy(x))->magnitude
          let b = add(copy(x), copy(hd))->magnitude
          max(a, max(b, acc))
        })
        getMax(tl, newmax)
      }
    }
  }
  Js.log2("Part 2:", getMax(nums, 0))
}

let input = [
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
  "[[[5,[2,8]],4],[5,[[9,9],0]]]",
  "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
  "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
  "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
  "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
  "[[[[5,4],[7,7]],8],[[8,3],8]]",
  "[[9,3],[[9,9],[6,[4,9]]]]",
  "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
  "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]",
]

part1(input)
part2(input)
