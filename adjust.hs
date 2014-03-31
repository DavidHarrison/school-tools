-- file: adjust.hs

import Data.Char          (isDigit)
import Data.Functor       ((<$>))
import Data.List          (transpose)
import System.Environment (getArgs)
import System.IO          (readFile, writeFile)
import Text.CSV           (parseCSV, printCSV, CSV)

main :: IO ()
main = inFile >>= readFile
       >>= (printCSV <$>)
           . (\s -> volumes >>= return . (\vs -> timeToVolume vs s))
           . init . filter isNumberPair . (either (\x -> []) id)
           . parseCSV "Titration2Raw.csv"
       >>= (\s -> outFile >>= (\f -> writeFile f s))
  where inFile        = (!! 0) <$> getArgs
        outFile       = (!! 1) <$> getArgs
        volumes       = volumeInitial
                        >>= (\vi -> volumeFinal
                        >>= return . (\vf -> (vi,vf)))
        volumeInitial = read . (!! 2) <$> getArgs
        volumeFinal   = read . (!! 3) <$> getArgs

timeToVolume :: (Double,Double) -> CSV -> CSV
timeToVolume (vi,vf) rs = transpose $ [volumes,phs]
  where
    volumes :: [String]
    volumes = map (show . (volume vi vf timeInterval) . read) times
    timeInterval :: Double
    timeInterval = (read $ last $ times) - (read $ head $ times)
    times :: [String]
    times = head $ transpose pairs
    phs :: [String]
    phs = last $ transpose pairs
    pairs = filter isNumberPair rs

-- vi: initial volume
-- vf: final volume
-- ti: time interval
-- t: time
volume :: Double -> Double -> Double -> Double -> Double
volume vi vf ti t = vi * (1 - (vf / vi)**(t / ti))

isNumberPair :: [String] -> Bool
isNumberPair = all isNumber
  where
    isNumber :: String -> Bool
    isNumber = all (\x -> isDigit x || x == '.' || x == ' ')
