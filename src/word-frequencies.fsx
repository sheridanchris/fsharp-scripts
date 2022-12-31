open System
open System.Text.RegularExpressions

let lower (str: string) = str.ToLower() 

let words (str: string) =
  Regex.Matches(str, @"[\w|']+")
  |> Seq.map (fun m -> m.Value)
  |> Seq.toList

let frequencies words =
  words
  |> List.map lower
  |> List.countBy id

let mostUsedWords n words =
  words
  |> frequencies
  |> List.sortByDescending snd
  |> List.take n

let mostUsedWordsByLength n words =
  mostUsedWords n words
  |> List.groupBy snd
  |> List.map (fun (length, wordsWithFrequencies) -> length, List.map fst wordsWithFrequencies)

let songLyrics =
  """
  Hello darkness, my old friend
  I've come to talk with you again
  Because a vision softly creeping
  Left its seeds while I was sleeping
  And the vision that was planted in my brain
  Still remains
  Within the sound of silence
  In restless dreams, I walked alone
  Narrow streets of cobblestone
  'Neath the halo of a street lamp
  I turned my collar to the cold and damp
  When my eyes were stabbed by the flash of a neon light
  That split the night
  And touched the sound of silence
  And in the naked light, I saw
  Ten thousand people, maybe more
  People talking without speaking
  People hearing without listening
  People writing songs that voices never shared
  And no one dared
  Disturb the sound of silence
  "Fools" said I, "You do not know
  Silence like a cancer grows
  Hear my words that I might teach you
  Take my arms that I might reach you"
  But my words, like silent raindrops fell
  And echoed in the wells of silence
  And the people bowed and prayed
  To the neon god they made
  And the sign flashed out its warning
  In the words that it was forming
  Then the sign said, "The words on the prophets are written on the subway walls
  In tenement halls"
  And whispered in the sound of silence
  """

words songLyrics
|> mostUsedWordsByLength 20