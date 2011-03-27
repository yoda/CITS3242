// Learn more about F# at http://fsharp.net


open System
open System.IO 

type fileContents = string
type line         = string
type wordstr      = string

type lineNumber   = int
type indexEntry   = wordstr * (lineNumber list)  // Parens not needed
type index        = indexEntry list

/// the width of an index-entry
let entryLength = 20

/// splits a string into a list of strings, one for each line
let lines (str:string) = Array.toList (str.Split([|Environment.NewLine|], StringSplitOptions.None))

/// splits a string into a list of string, one for each word
let words (str:string) = 
    Array.toList (str.Split ([|' ';'\t';'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries))

/// Takes two lists, returns a list of pairs: first the head of each list,
/// then the second of each, etc. 
let zip = List.zip

/// compares pairs, returns -ve, 0 or +ve depending on which is less.
let compare (w1:wordstr, ln1:lineNumber) (w2:wordstr,ln2:lineNumber) =
    match w1.CompareTo(w2) with
    | 0 -> ln1.CompareTo(ln2)
    | cmpW -> cmpW
        
/// Sort a list of (word * lineNumber)
let sort = List.sortWith compare 

/// Remove duplicates from a sorted list
let rec loseAdjacentDuplicates = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::xs -> let ys = loseAdjacentDuplicates (x2::xs)
                    if x1=x2 then ys else x1::ys


// *******************
// Put your code here.
// ********************

let numberLines fileContents = zip (lines fileContents) [1 .. List.length (lines fileContents)]
    
let numberWords fileContents = List.concat (List.filter (fun x -> not (List.isEmpty x)) [for (line, number) in (numberLines fileContents) -> [for word in words line -> (word, number) ]])

let distinctfsts orderedNumberWordsContents = loseAdjacentDuplicates [for (word, number) in orderedNumberWordsContents ->  word]

let snds x xs = [for (w, n) in (loseAdjacentDuplicates [for (word, number) in xs do if word = x then yield (word, number)]) -> n ]

let combineWords xs = [for w in (distinctfsts xs) -> (w, snds w xs)]

let makeIndex fileContents = combineWords (sort (numberWords fileContents))

/// repeat a string a given number of times
let rec repeatStr s = function
    | 0 -> ""
    | n -> s + repeatStr s (n-1)

/// Converts an entry to a string
let showEntry (w:wordstr, is:lineNumber list) =
    let isString = String.concat "," (List.map string is) 
    w + repeatStr " " (entryLength-w.Length) + isString

/// Converts an index to a string.
let showIndex idx = String.concat Environment.NewLine (List.map showEntry idx)

/// takes a filename f and outputs an index for it to f.index
let main filename = 
    let input = File.ReadAllText(filename)
    File.WriteAllText(filename+".index", showIndex (makeIndex input))

// Replace the following with the actual path to the input file
// The "@" here indicates a literal string - one where \ just means itself
// rather than being used for \n, \t, \", etc.

//main @"\\uggp.csse.uwa.edu.au\rowan\teaching\paradigms09\Lab2Index\inp";;
main @"C:\Users\yoda\Desktop\CITS3242\Lab02\Lab02\inp.txt";;

