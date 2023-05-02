{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Protolude

main :: IO ()
main = do
    print "Enter a command"
    command <- getLine
    performCommand command

dbName = "tools.db"

performCommand :: Text -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "addtool" = promptAndAddTool >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry command not found" >> main

data Tool = Tool
    { toolId :: Int
    , name :: Text
    , description :: Text
    , lastReturned :: Day
    , timesBorrowed :: Int
    }
    deriving (Generic, Show)

deriving anyclass instance FromRow Tool
deriving anyclass instance ToRow Tool

data User = User
    { userId :: Int
    , userName :: Text
    }
    deriving (Generic, Show)

deriving anyclass instance FromRow User
deriving anyclass instance ToRow User

withConn :: Text -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open $ toS dbName
    action conn
    close conn

withConnToolsDb = withConn dbName

addUser :: Text -> IO ()
addUser userName = withConnToolsDb $ \conn -> do
    execute
        conn
        "insert into users (username) values (?)"
        (Only userName)
    print "user added"

addTool :: Text -> Text -> Day -> Int -> IO ()
addTool n d l t = withConnToolsDb $ \conn -> do
    execute
        conn
        [sql|
        insert into tools (name, description, lastReturned, timesBorrowed)
        values (?,?,?,?)
    |]
        (n, d, l, t)
    print "tool added"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConnToolsDb $ \conn -> do
    execute
        conn
        "insert into checkedout (user_id,tool_id) values (?,?)"
        (userId, toolId)

printUsers :: IO ()
printUsers = withConnToolsDb $ \conn -> do
    resp <- query_ conn "select * from users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConnToolsDb $ \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "select * from tools;"

printAvailable :: IO ()
printAvailable =
    printToolQuery
        [sql|
    select * from tools
    where id not in
        (select tool_id from checkedout)
    |]

printCheckedout :: IO ()
printCheckedout =
    printToolQuery
        [sql|
    select * from tools
    where id in
        (select tool_id from checkedout)
    |]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <-
        query
            conn
            [sql|
        select * from tools
        where id = (?);
        |]
            (Only toolId) ::
            IO [Tool]
    return $ head resp

updateTool :: Tool -> Day -> Tool
updateTool tool day =
    tool
        { lastReturned = day
        , timesBorrowed = 1 + timesBorrowed tool
        }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConnToolsDb $ \conn -> do
    execute
        conn
        [sql|
    update tools set
        lastReturned = ?,
        timesBorrowed = ?
    where id = ?;
    |]
        ( lastReturned tool
        , timesBorrowed tool
        , toolId tool
        )
    print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConnToolsDb $ \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConnToolsDb $ \conn -> do
    execute
        conn
        [sql|
        delete from checkedout
        where tool_id = (?)
    |]
        (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- readMaybe <$> getLine
    print "Enter the id of the tool"
    toolId <- readMaybe <$> getLine
    if all isJust [userId, toolId]
        then
            let [uid, tid] = catMaybes [userId, toolId]
             in checkout uid tid
        else promptAndCheckout

promptAndCheckin :: IO ()
promptAndCheckin = do
    print "Enter the id of tool"
    toolId <- readMaybe <$> getLine
    if isJust toolId
        then
            let [tid] = catMaybes [toolId]
             in checkinAndUpdate tid
        else promptAndCheckin

promptAndAddTool :: IO ()
promptAndAddTool = do
    print "Enter new tool name"
    toolName <- getLine
    print "Enter new tool description"
    toolDescription <- getLine
    day <- utctDay <$> getCurrentTime
    addTool toolName toolDescription day 0
