{-# LANGUAGE PatternGuards #-}

import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile );
import Data.Char (isDigit)
import Data.List


partTwo :: String -> IO Integer
partTwo filename = do 
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
    else case isNumberWord (x : xs) of
        Left y -> y
        Right () -> getFirstDigit xs 


getLastDigit :: String -> Char -> Char
getLastDigit [] lastFoundNumber = lastFoundNumber
getLastDigit (x : xs) lastFoundNumber = 
    if isDigit x then getLastDigit xs x
    else case isNumberWord (x : xs) of 
        Left y -> getLastDigit xs y
        Right () -> getLastDigit xs lastFoundNumber


isNumberWord :: String -> Either Char ()
isNumberWord x | Just restOfString <- stripPrefix "zero" x   = Left '0'
isNumberWord x | Just restOfString <- stripPrefix "one" x    = Left '1'
isNumberWord x | Just restOfString <- stripPrefix "two" x    = Left '2'
isNumberWord x | Just restOfString <- stripPrefix "three" x  = Left '3'
isNumberWord x | Just restOfString <- stripPrefix "four" x   = Left '4'
isNumberWord x | Just restOfString <- stripPrefix "five" x   = Left '5'
isNumberWord x | Just restOfString <- stripPrefix "six" x    = Left '6'
isNumberWord x | Just restOfString <- stripPrefix "seven" x  = Left '7'
isNumberWord x | Just restOfString <- stripPrefix "eight" x  = Left '8'
isNumberWord x | Just restOfString <- stripPrefix "nine" x   = Left '9'
isNumberWord x = Right ()
