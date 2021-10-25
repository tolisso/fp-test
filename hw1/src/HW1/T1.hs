module HW1.T1 where
import GHC.Num

data Day = Monday | Tuesday | Wednesday 
    | Thursday | Friday | Saturday | Sunday deriving (Show, Eq) 

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day

afterDays 0 day = day
afterDays num day = afterDays (num - 1) (nextDay day)

isWeekend :: Day -> Bool

isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: Day -> Natural

daysToParty Friday = 0
daysToParty day = daysToParty (nextDay day) + 1
