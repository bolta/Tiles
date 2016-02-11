module Time where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- | 現在時刻の年月日時分秒を得る
getCurrentTimeYmdhms :: IO (Integer, Int, Int, Int, Int, Int)
getCurrentTimeYmdhms = do
	cur <- getCurrentTime
	tz <- getTimeZone cur
	let
		lTime = utcToLocalTime tz cur
		lDay = localDay lTime
		lTod = localTimeOfDay lTime
		(y, m, d) = toGregorian lDay
		h = todHour lTod
		i = todMin lTod
		s = toInt $ todSec lTod 
		toInt pico = pico `div'` 1 :: Int -- TODO 他にやりようはないのか？
	
	return (y, m, d, h, i, s)

