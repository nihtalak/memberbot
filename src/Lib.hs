{-# LANGUAGE OverloadedStrings #-}

module Lib (core) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Memory = Memory Int String String T.Text

instance Show Memory where
  show (Memory _ _ _ value) = show value

instance FromRow Memory where
  fromRow = Memory <$> field <*> field <*> field <*> field

instance ToRow Memory where
  toRow (Memory id_ user key value) = toRow (id_, user, key, value)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

createMemory :: Connection -> Memory -> IO ()
createMemory conn mem =
  execute conn "INSERT INTO memories (id, user, key, value) VALUES (?, ?, ?, ?)" mem

deleteMemory :: Connection -> String -> String -> IO ()
deleteMemory conn user key =
  execute conn "DELETE FROM memories WHERE user = ? AND key = ?" (user, key)

getMemory :: Connection -> String -> String -> IO (Maybe Memory)
getMemory conn user key =
  head' <$> query conn "SELECT * FROM memories WHERE user = ? AND key = ? LIMIT 1" (user, key)

toResponse :: Maybe Memory -> String
toResponse Nothing = "didnt member :("
toResponse (Just m) = show m

core :: IO ()
core = do
  conn <- open "dev.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS memories (id INTEGER PRIMARY KEY, user VARCHAR, key VARCHAR, value TEXT)"
  createMemory conn $ Memory 1 "sender" "cwar" "COLD WAR"
  mem <- getMemory conn "sender" "cwar"
  putStrLn $ toResponse mem
  deleteMemory conn "sender" "cwar"
  close conn
