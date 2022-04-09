{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Database.PostgreSQL.Simple (Connection)

import Data.List (genericLength)
import Data.Time (addUTCTime, diffUTCTime)

import Control.Monad.IO.Class (liftIO)

import System.Environment (getEnv)
import System.Directory (doesFileExist)

import Configuration.Dotenv

import Database
import HistoryItem
import TimeUtils

average :: (Real a, Integral b) => [a] -> b
average xs = round $ realToFrac (sum xs) / genericLength xs

resolveCors = middleware simpleCors
logRequest = middleware logStdoutDev

tryGetParam paramName = (
  do
    result <- param paramName
    return $ Just result
  ) `rescue` (
    \_ -> liftIO $ return Nothing
  )

server :: Connection -> ScottyM()
server conn = do
  resolveCors
  logRequest
  middleware $ staticPolicy (addBase "static")

  get "/" $ do
    file "./static/index.html"

  get "/history" $ do
    from <- tryGetParam "from"
    to <- tryGetParam "to"
    history <- 
      case (from, to) of
        (Just periodBegin, Just periodEnd) -> liftIO (getPeriodData conn periodBegin periodEnd)
        (_, _) -> liftIO (getDefaultData conn)
    json history

  post "/history" $ do
    recordedMinuteData <- jsonData :: ActionM [Int]
    currentTime <- liftIO getMoscowTime
    let maxValue = maximum recordedMinuteData
        minValue = minimum recordedMinuteData
        avgValue = average recordedMinuteData
        historyItem = HistoryItem currentTime maxValue minValue avgValue
    newItem <- liftIO (insertHistory conn historyItem)
    json newItem

  notFound $ do
    file "./static/notfound.html"

loadEnvironmentFileIfExists :: IO [(String, String)]
loadEnvironmentFileIfExists = do
  envFileExists <- doesFileExist ".env"
  if envFileExists then
    loadFile defaultConfig
  else
    return []

main :: IO ()
main = do
  loadEnvironmentFileIfExists
  dbHost <- getEnv "DATABASE_HOST"
  dbName <- getEnv "DATABASE_NAME"
  dbUser <- getEnv "DATABASE_USER"
  dbPassword <- getEnv "DATABASE_PASSWORD"
  port <- read <$> getEnv "PORT"
  dbconnection <- connectToDatabase dbHost dbName dbUser dbPassword
  scotty port $ server dbconnection