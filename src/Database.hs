module Database (
  connectToDatabase,
  getDefaultData,
  getPeriodData,
  insertHistory
) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock
import Data.String (fromString)
import HistoryItem
import TimeUtils
import qualified Data.ByteString.Char8 as C (pack)

insertHistoryQuery = fromString "insert into history values (?, ?, ?, ?) returning datetime, maxvalue, minvalue, avgvalue"

connectToDatabase :: String -> String -> String -> String -> IO Connection
connectToDatabase dbHost dbName dbUser dbPassword= do
  let database = "host='" ++ dbHost ++ "' user='" ++ dbUser ++ "' dbname='" ++ dbName ++ "' password='" ++ dbPassword ++ "'"
  connectPostgreSQL (C.pack database)

getDefaultData :: Connection -> IO [HistoryItem]
getDefaultData conn = do
  currentTime <- getMoscowTime
  let hourAgo = addLocalTime (-3600 :: NominalDiffTime) currentTime
  getPeriodData conn (show $ hourAgo) (show currentTime)
  
getPeriodHistoryQuery :: String -> String -> Query
getPeriodHistoryQuery from to = 
  fromString $ "select datetime, maxValue, minValue, avgValue from history where datetime >= '" ++ from ++ "'::timestamp and datetime < '" ++ to ++ "'::timestamp;"
  
getPeriodData :: Connection -> String -> String -> IO [HistoryItem]
getPeriodData conn from to = do
  query_ conn (getPeriodHistoryQuery from to) :: IO [HistoryItem]

insertHistory :: Connection -> HistoryItem -> IO HistoryItem
insertHistory conn item = do
  res <- query conn insertHistoryQuery item :: IO [HistoryItem]
  return item