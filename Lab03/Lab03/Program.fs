// Learn more about F# at http://fsharp.net



let safeDiv x = function 0 -> None 
                       | y -> Some(x/y)

// 1 a).
// fun is used to define a lambda expression, an anonymous function.
let optMapBinary = fun f x y -> 
    match (x, y) with
        | (Some xx, Some yy) -> Some(f xx yy)
        |  _ -> None

// 1 b).
let optPlus x y = optMapBinary (+) x y

let optTimes x y = optMapBinary (*) x y

// 1 c).

let safeCalc x y = 
    let halfcalc = optPlus (safeDiv x y) (safeDiv y x)
    optTimes (optTimes halfcalc halfcalc) halfcalc

// 2 a).
type 'a tree = Leaf
             | Node of ('a tree) * 'a * ('a tree) // left-child, key, right-child

let rec isElementOf searchKey = function
    | Leaf -> false
    | Node (left, nodeKey, right) ->
        if nodeKey = searchKey then true
        elif nodeKey < searchKey then isElementOf searchKey left
        else isElementOf searchKey right

let rec insert newKey =
    let rec inserter theNewKey = function
        | Leaf -> Node (Leaf, theNewKey, Leaf)
        | Node (left, nodeKey, right) ->
            if theNewKey < nodeKey
            then Node (inserter theNewKey left, nodeKey, right)
            else Node (left, nodeKey, inserter theNewKey right)
    function
        | Leaf as theLeaf -> inserter newKey theLeaf
        | Node (left, nodeKey, right) as theNode -> 
          if (isElementOf newKey theNode) = true then theNode
             else inserter newKey theNode
