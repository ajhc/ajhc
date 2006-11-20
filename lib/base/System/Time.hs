{-# OPTIONS_JHC -fffi #-}
module Time (
    ClockTime,
    Month(..),
    Day(..),
    CalendarTime(..),
    TimeDiff(..),
    getClockTime,
    addToClockTime,
    diffClockTimes,
    toCalendarTime,
    toUTCTime,
    toClockTime,
    calendarTimeToString,
    formatCalendarTime
    ) where

import Data.Char
import Data.Ix
import Foreign.C.Types
import Foreign.Ptr
import System.Locale
import System.IO.Unsafe

data ClockTime = TOD Integer Integer -- Implementation-dependent
    deriving(Eq,Ord)

-- When a ClockTime is shown, it is converted to a CalendarTime in the current
-- timezone and then printed.  FIXME: This is arguably wrong, since we can't
-- get the current timezone without being in the IO monad.

instance Show ClockTime where
    showsPrec _ t = showString (calendarTimeToString
	  			 (unsafePerformIO (toCalendarTime t)))

data Month =  January   | February | March    | April
           |  May       | June     | July     | August
           |  September | October  | November | December
           deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Ix Month where
    range (x,y) = enumFromTo x y
    index (x,y) i
        | y < x || i < x || i > y = error "Time.Month.Ix: index out of range"
        | otherwise = fromEnum i - fromEnum x
    inRange (x,y) i = y >= x && (not $ i < x || i > y)
    rangeSize (x,y)
        | y < x = 0
        | otherwise = fromEnum y - fromEnum x



data Day   =  Sunday | Monday  | Tuesday  | Wednesday | Thursday
           |  Friday | Saturday
           deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Ix Day where
    range (x,y) = enumFromTo x y
    index (x,y) i
        | y < x || i < x || i > y = error "Time.Day.Ix: index out of range"
        | otherwise = fromEnum i - fromEnum x
    inRange (x,y) i = y >= x && (not $ i < x || i > y)
    rangeSize (x,y)
        | y < x = 0
        | otherwise = fromEnum y - fromEnum x

data CalendarTime = CalendarTime {
    ctYear   :: Int,
    ctMonth  :: Month,
    ctDay, ctHour, ctMin, ctSec  :: Int,
    ctPicosec :: Integer,
    ctWDay    :: Day,
    ctYDay       :: Int,
    ctTZName   :: String,
    ctTZ          :: Int,
    ctIsDST :: Bool
    } deriving (Eq, Ord, Read, Show)

data TimeDiff = TimeDiff {
    tdYear, tdMonth, tdDay, tdHour, tdMin, tdSec :: Int,
    tdPicosec      :: Integer
    } deriving (Eq, Ord, Read, Show)


-- Functions on times
getClockTime         :: IO ClockTime
getClockTime = do
    secs <- c_time nullPtr -- can't fail, according to POSIX
    return (TOD (ctimeToInteger secs) 0)

foreign import primitive "integralCast" ctimeToInteger :: CTime -> Integer
   -- -----------------------------------------------------------------------------
-- | converts an internal clock time to a local time, modified by the
-- timezone and daylight savings time settings in force at the time
-- of conversion.  Because of this dependence on the local environment,
-- 'toCalendarTime' is in the 'IO' monad.

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime =  toCalTime False

-- | converts an internal clock time into a 'CalendarTime' in standard
-- UTC format.

toUTCTime :: ClockTime -> CalendarTime
toUTCTime      =  unsafePerformIO . toCalTime True

toCalTime :: Bool -> ClockTime -> IO CalendarTime
toCalTime = error "toCalTime"

toClockTime          :: CalendarTime -> ClockTime
toClockTime = error "toClockTime"

calendarTimeToString    :: CalendarTime -> String
calendarTimeToString    =  formatCalendarTime defaultTimeLocale "%c"

-- -----------------------------------------------------------------------------
-- | @'addToClockTime' d t@ adds a time difference @d@ and a
-- clock time @t@ to yield a new clock time.  The difference @d@
-- may be either positive or negative.

addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec)
	       (TOD c_sec c_psec) =
	let
	  sec_diff = toInteger sec +
                     60 * toInteger min +
                     3600 * toInteger hour +
                     24 * 3600 * toInteger day
	  cal      = toUTCTime (TOD (c_sec + sec_diff) (c_psec + psec))
                                                       -- FIXME! ^^^^
          new_mon  = fromEnum (ctMonth cal) + r_mon
	  month' = fst tmp
	  yr_diff = snd tmp
          tmp
	    | new_mon < 0  = (toEnum (12 + new_mon), (-1))
	    | new_mon > 11 = (toEnum (new_mon `mod` 12), 1)
	    | otherwise    = (toEnum new_mon, 0)

	  (r_yr, r_mon) = mon `quotRem` 12

          year' = ctYear cal + year + r_yr + yr_diff
	in
	toClockTime cal{ctMonth=month', ctYear=year'}

-- | @'diffClockTimes' t1 t2@ returns the difference between two clock
-- times @t1@ and @t2@ as a 'TimeDiff'.

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
-- diffClockTimes is meant to be the dual to `addToClockTime'.
-- If you want to have the TimeDiff properly splitted, use
-- `normalizeTimeDiff' on this function's result
--
-- CAVEAT: see comment of normalizeTimeDiff
diffClockTimes (TOD sa pa) (TOD sb pb) =
    noTimeDiff{ tdSec     = fromIntegral (sa - sb)
                -- FIXME: can handle just 68 years...
              , tdPicosec = pa - pb
              }

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0


formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt ct@(CalendarTime year mon day hour min sec sdec
                                           wday yday tzname _ _) =
        doFmt fmt
  where doFmt ('%':c:cs) = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        to12 :: Int -> Int
        to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'

        decode 'A' = fst (wDays l  !! fromEnum wday)
        decode 'a' = snd (wDays l  !! fromEnum wday)
        decode 'B' = fst (months l !! fromEnum mon)
        decode 'b' = snd (months l !! fromEnum mon)
        decode 'h' = snd (months l !! fromEnum mon)
        decode 'C' = show2 (year `quot` 100)
        decode 'c' = doFmt (dateTimeFmt l)
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day
        decode 'e' = show2' day
        decode 'H' = show2 hour
        decode 'I' = show2 (to12 hour)
        decode 'j' = show3 yday
        decode 'k' = show2' hour
        decode 'l' = show2' (to12 hour)
        decode 'M' = show2 min
        decode 'm' = show2 (fromEnum mon+1)
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l)
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec
--        decode 's' = ...                -- Implementation-dependent
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7)
        decode 'u' = show (let n = fromEnum wday in
                           if n == 0 then 7 else n)
        decode 'V' =
            let (week, days) =
                   (yday + 7 - if fromEnum wday > 0 then
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1
                       else if week == 0 then 53 else week)

        decode 'W' =
            show2 ((yday + 7 - if fromEnum wday > 0 then
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)
        decode 'X' = doFmt (timeFmt l)
        decode 'x' = doFmt (dateFmt l)
        decode 'Y' = show year
        decode 'y' = show2 (year `rem` 100)
        decode 'Z' = tzname
        decode '%' = "%"
        decode c   = [c]

show2, show2', show3 :: Int -> String
show2 x = [intToDigit (x `quot` 10), intToDigit (x `rem` 10)]

show2' x = if x < 10 then [ ' ', intToDigit x] else show2 x

show3 x = intToDigit (x `quot` 100) : show2 (x `rem` 100)


foreign import unsafe ccall "time.h time" c_time :: Ptr CTime -> IO CTime

