// just implementing some list functions that exist in the standard library (map, filter, ...)

let rec fold folder state list =
  match list with
  | [] -> state
  | x :: xs ->
    let state = folder state x
    fold folder state xs

let filter f list =
  let rec loop f acc remaining =
    match remaining with
    | [] -> acc
    | x :: xs when f x -> loop f (x :: acc) xs 
    | x :: xs -> loop f acc xs 

  loop f [] list

let map f list =
  let rec loop f acc remaining =
    match remaining with
    | [] -> acc
    | x :: xs -> loop f (f x :: acc) xs 

  loop f [] list

let sum = fold (+) 0 [0..5]
let evenNumbers = filter (fun x -> x % 2 = 0) [0..100]
let doubles = map (fun x -> x * 2) [0..100]