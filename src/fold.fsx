let assertTrue cond message =
  if cond then () else failwith message

let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (list: 'T list) =
  match list with
  | [] -> state
  | x :: xs ->
    let state = folder state x
    fold folder state xs

let result = fold (+) 0 [1..5] // 1 + 2 + 3 + 4 + 5 = 15
assertTrue (result = 15) "Result <> 15"