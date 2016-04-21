-- openFile, closeFile, fEOF, fGetLine , and writeFile
import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  case x of
    "" => pure []
    _ => do xs <- readToBlank
            pure (x :: xs)

readAndSave : IO ()
readAndSave = do
  x <- readToBlank
  putStrLn "Enter file name: "
  filename <- getLine
  Right () <- writeFile filename (unlines x) | Left err => putStrLn (show err)
  pure ()


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right f <- openFile filename Read
                               | Left err => pure (_ ** [])
                           Right contents <- fileContents f
                               | Left err => pure (_ ** [])
                           closeFile f
                           pure contents
    where fileContents : File -> IO (Either FileError (n ** Vect n String))
          fileContents f = do eof <- fEOF f
                              if eof then pure (Right (_ ** [])) else do
                                 Right str <- fGetLine f
                                    | Left err => pure (Left err)
                                 Right (_ ** rest) <- fileContents f
                                    | Left err => pure (Left err)
                                 pure (Right (_ ** str :: rest))
