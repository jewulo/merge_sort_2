// For more information see https://aka.ms/fsharp-console-apps

// Merge Sort code from chatGPT
open System;

let rec mergeSort lst = 
    let rec merge left right =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x < y then x :: merge xs right
            else y :: merge left ys
    match lst with
    | []  -> []
    | [x] -> [x]
    | _   ->
        let rec split n left right =
            match n, right with
            | _, [] -> left, []
            | _, x::xs when n = 0 -> List.rev left, right
            | _, x::xs -> split (n-1) (x::left) xs
        let length = List.length lst
        let left, right = split (length / 2) [] lst
        merge (mergeSort left) (mergeSort right)

printfn "merge sort test"
printfn "%A" (mergeSort [ 10; 7; 0; 9; 3; 8; 4; 1; 5; 2; 6 ])
mergeSort [ 10; 7; 0; 9; 3; 8; 4; 1; 5; 2; 6 ] |> printfn "%A"

let rec merge left right =
    match left, right with
    | [], _ -> right
    | _, [] -> left
    | x::xs, y::ys ->
        if x < y then x :: merge xs right
        else y :: merge left ys

printfn ""
printfn "merge test"
merge [8; 4; 1; 5; 2; 6] [10; 7; 0; 9; 3] |> printfn "%A"
merge [10; 7; 0; 9; 3] [8; 4; 1; 5; 2; 6] |> printfn "%A"

