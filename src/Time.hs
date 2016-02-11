module Time where

import Data.Fixed
import Data.List (intercalate)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

type Ymdhms = (Integer, Int, Int, Int, Int, Int)

-- | 現在時刻の年月日時分秒を得る
getCurrentTimeYmdhms :: IO Ymdhms
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

formatDateTime1 :: Ymdhms -> String
formatDateTime1 (y, m, d, h, i, s) =
	let
		y' = show y
		[m', d', h', i', s'] = map padWith0 [m, d, h, i, s]
	in
		intercalate "-" [y', m', d']
			++ " "
			++ intercalate ":" [h', i', s']

padWith0 :: Int -> String
padWith0 i = if i < 10 then '0' : show i else show i


