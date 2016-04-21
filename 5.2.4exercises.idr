import System

readNumber : IO (Maybe Nat)
readNumber = do
             input <- getLine
             if all isDigit (unpack input)
              then pure (Just (cast input))
              else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn (show guesses ++ " guesses so far")
  putStr "Guess a number between 1 and 100: "
  validNum <- readNumber
  case validNum of
        Nothing => do putStrLn "Invalid input"
                      guess target guesses
        (Just guessNum) => if guessNum < target
                            then do putStrLn "To low try again"
                                    guess target (guesses + 1)
                            else if guessNum > target
                              then do putStrLn "To high try again"
                                      guess target (guesses + 1)
                              else putStrLn "Corret"

main : IO ()
main = do secs <- time
          guess (cast (secs `mod` 101)) 0

myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do
  putStr prompt
  input <- getLine
  putStr (onInput input)
  myRepl prompt onInput

myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith acc prompt onInput = do
  putStr prompt
  input <- getLine
  case onInput acc input of
       Nothing => return ()
       Just (out, acc') => do
        putStr out
        myReplWith acc' prompt onInput
