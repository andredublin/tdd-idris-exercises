module Main

palindrome : Nat -> String -> Bool
palindrome x str =
  let rev_str = toLower (reverse str) in
    rev_str == toLower str && length str > x

checkPalindrome : String -> String
checkPalindrome str = "" ++ show (palindrome 5 str) ++ "\n"

counts : String -> (Nat, Nat)
counts str =
  let num_words = length (words str)
      num_chars = length str in
        (num_words, num_chars)

checkCounts : String -> String
checkCounts str = "" ++ show (counts str) ++ "\n"

top_ten : Ord a => List a -> List a
top_ten x = take 10 (reverse (sort x))

over_length : Nat -> List String -> Nat
over_length num strs =
  let top_words = filter_nums num (word_lengths strs) in
    top_words
  where
  word_lengths : List String -> List Nat
  word_lengths strs = map length strs

  filter_nums : Nat -> List Nat -> Nat
  filter_nums num nums = List.length (filter (num <) nums)

main : IO ()

main = repl "Enter a string: " checkPalindrome
