import Data.List

mycrazyoperation = (intersperse '.') . (intercalate " ")

removeAllTwo = (\\[2]) . nub