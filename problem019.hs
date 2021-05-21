
type Year = Integer
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Enum, Bounded)
type Day = Integer
data Date = Date Year Month Day


daysPerMonth :: (Integral a) => Year -> Month -> a
daysPerMonth year month = case month of
    January -> 31
    February -> if isLeapYear year then 29 else 28
    March -> 31
    April -> 30
    May -> 31
    June -> 30
    July -> 31
    August -> 31
    September -> 30
    October -> 31
    November -> 30
    December -> 31
    where
        isLeapYear year
            | year `mod` 400 == 0 = True
            | year `mod` 100 == 0 = False
            | year `mod` 4   == 0 = True
            | otherwise           = False


addOneWeek :: Date -> Date
addOneWeek (Date year month day)
    | daysLeftInMonth >= 7  = Date year month (day + 7)
    | month /= maxBound     = Date year (succ month) (7 - daysLeftInMonth)
    | otherwise             = Date (year + 1) minBound (7 - daysLeftInMonth)
    where daysLeftInMonth = (daysPerMonth year month) - day


countWeeks :: Date -> [Date]
countWeeks start = start : (countWeeks . addOneWeek $ start)


problem19 = length . filter (\(Date _ _ day) -> day == 1) $ allSundays
    where allSundays = takeWhile (\(Date year _ _) -> year <= 2000) $ countWeeks firstSunday
          firstSunday = Date 1901 January 6


main = print problem19
