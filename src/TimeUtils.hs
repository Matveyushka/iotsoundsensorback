module TimeUtils (
  addLocalTime,
  getMoscowTime
) where

import Data.Time.LocalTime
import Data.Time.Clock

addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

getMoscowTime :: IO LocalTime
getMoscowTime = do
  currentTime <- getCurrentTime
  let moscowTimeZone = hoursToTimeZone 3
  return $ utcToLocalTime moscowTimeZone currentTime