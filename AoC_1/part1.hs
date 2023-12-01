import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile );
import Data.Char (isDigit)


partOne :: String -> IO Integer
partOne filename = do 
    lines <- getInputs filename
    return (foldl (\x y -> x + read (processLine y) :: Integer) 0 lines)


getInputs :: String -> IO [String]
getInputs filename = do
    file <- openFile filename ReadMode
    contents <- hGetContents file
    return (lines contents)


processLine :: String -> String
processLine line = getFirstDigit line : [getLastDigit line '0']


getFirstDigit :: String -> Char
getFirstDigit [] = '0'
getFirstDigit (x : xs) = 
    if isDigit x then x
    else getFirstDigit xs 


getLastDigit :: String -> Char -> Char
getLastDigit [] lastFoundInteger = lastFoundInteger
getLastDigit (x : xs) lastFoundInteger = 
    if isDigit x then getLastDigit xs x
    else getLastDigit xs lastFoundInteger
