module Main

import Data.Vect

-- DataStore is the type constructor
data DataStore : Type where
  -- MkData is the data constructor, and gives the canonical way of constructing a data store
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

data Command = Add String
              | Get Integer
              | Quit

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store)
  where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                (cmd, args) => parseCommand cmd args

search : Nat -> (items : Vect n String) -> (str : String) -> String
search k [] str = ""
search k (x :: xs) str = let found = search (k + 1) xs str in
  if isInfixOf str x
    then show k ++ ": " ++ x ++ "\n" ++ found
    else found

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                        case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get item) => getEntry item store
                                Just Quit => Nothing

main : IO ()

main = replWith (MkData _ []) "Command: " processInput
