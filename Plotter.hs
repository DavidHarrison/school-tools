{-
 - File: Plotter.hs
 - tool to plot (x,y) data points from CSV file with a best-fit line
 - (mostly used for Chemistry)
 -}
import Text.CSV (CSV, Record, parseCSVFromFile)
import Graphics.EasyPlot (plot, Graph2D(..), Option(Title, Style, Color),
                          Style(Points, Lines), TerminalType(..),
                          Color(Black), Option2D(Range))
import Data.Char (isNumber)
import Data.Either.Utils (fromRight)
import Data.Maybe (fromMaybe, fromJust)
import Statistics.LinearRegression (linearRegression)
import Data.Vector.Unboxed (fromList)
import Data.List (transpose, stripPrefix)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import System.Exit (exitFailure)

main :: IO Bool
main = getArgs
       >>= return . fromJust . procArgs
       >>= fromFile

procArgs :: [String] -> Maybe (FilePath,TerminalType)
procArgs (fi:tt:fo:[]) = Just (fi, terminalType tt $ Just fo)
procArgs (fi:tt:[]) = Just (fi,terminalType tt Nothing)
procArgs _ = Nothing

terminalType :: String -> Maybe FilePath -> TerminalType
terminalType "Aqua" _         = Aqua
terminalType "Windows" _      = Windows
terminalType "X11" _          = X11
terminalType "PS" (Just f)    = PS f
terminalType "EPS" (Just f)   = EPS f
terminalType "PNG" (Just f)   = PNG f
terminalType "PDF" (Just f)   = PDF f
terminalType "SVG" (Just f)   = SVG f
terminalType "GIF" (Just f)   = GIF f
terminalType "JPEG" (Just f)  = JPEG f
terminalType "Latex" (Just f) = Latex f

usage :: String
usage = "plot <infile.csv> <TerminalType (X11, JPEG, Latex etc)> [<outfile>]"

getFileName :: String -> FilePath
getFileName s = fromMaybe s (stripExt s)

stripExt :: FilePath -> Maybe FilePath
stripExt f
  | length stripped == 0 = Nothing
  | otherwise = Just $ init stripped
    where stripped = reverse $ dropWhile (/= '.') $ reverse f


fromFile :: (FilePath, TerminalType) -> IO Bool
fromFile (f,tt) = getCSV f
                  >>= return . (map adjust) . points
                  >>= return . makeGraphs
                  >>= plot tt

adjust :: (Double,Double) -> (Double,Double)
adjust (x,y) = (x,y)

makeGraphs :: [(Double,Double)] -> [Graph2D Double Double]
makeGraphs ps = [points_plot, best_fit_line]
  where points_plot = Data2D [Title "", Style Points] [] ps
        best_fit_line = functionPlot ps

functionPlot :: [(Double,Double)] -> Graph2D Double Double
functionPlot ps = Function2D
                    [Title title, Style Lines, Color Black]
                    [Range (minimum xs) (maximum xs)]
                    lineFunction
  where
    title = (show beta) ++ "x + " ++ (show alpha)
    lineFunction = (\x -> alpha + beta * x)
    (alpha,beta) = linearRegression (fromList xs) (fromList ys)
    (xs,ys) = unzip ps

getCSV :: FilePath -> IO CSV
getCSV f = parseCSVFromFile f >>= return . fromRight

points :: CSV -> [(Double,Double)]
points = (\ps -> zip (ps !! 0) (ps !! 1)) . transpose . toDoubles
  where toDoubles = (map (map read)) . (filter areDoubles)

areDoubles :: [String] -> Bool
areDoubles = all isDouble

isDouble :: String -> Bool
isDouble s = all (`elem` "0123456789. ") s && length s > 0
