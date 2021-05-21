
onesWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teensWords = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tensWords = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numToWords :: (Integral a) => a -> String
numToWords x
    | x < 0     = ""
    | x < 10    = onesWords !! fromIntegral x
    | x < 20    = teensWords !! fromIntegral (x `mod` 10)
    | x < 100   = let (tens, remainder) = x `divMod` 10 in (tensWords !! fromIntegral tens) ++ if remainder == 0 then "" else "-" ++ (numToWords remainder)
    | x < 1000  = let (hundreds, remainder) = x `divMod` 100 in (numToWords hundreds) ++ " hundred" ++ if remainder == 0 then "" else " and " ++ (numToWords remainder)
    | x == 1000 = "one thousand"
    | otherwise = ""


problem17 = sum . map (length . filter (`elem` ['a'..'z']) . numToWords) $ [1..1000]

main = print problem17
