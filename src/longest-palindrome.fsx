open System

let isPalindrome (value: string) =
  let rev = Seq.rev value |> String.Concat
  value = rev

let longestPalindrome words =
  words
  |> List.distinct
  |> List.filter isPalindrome
  |> List.sortByDescending String.length
  |> List.tryHead


[ "racecar"; "hannah"; "deified"; "redivider" ]
|> longestPalindrome
|> Option.defaultValue "Sorry, I didn't find a palindrome :("
