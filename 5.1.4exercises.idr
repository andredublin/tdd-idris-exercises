printLarger : IO ()
printLarger = do putStr "Input string A: "
                 inputA <- getLine
                 putStr "Input string B: "
                 inputB <- getLine
                 let lenA = length inputA
                 let lenB = length inputB
                 if lenA > lenB then
                   putStrLn (show lenA)
                 else
                   putStrLn (show lenB)

printLarger' : IO ()
printLarger' = putStr "Input string A: " >>= \_ =>
                  getLine >>= \inputA =>
                  putStr "Input string B: " >>= \_ =>
                  getLine >>= \inputB =>
                    if length inputA > length inputB
                      then putStrLn (show (length inputA))
                      else putStrLn (show (length inputB))
