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
import Data.List (transpose)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO Bool
main = getArgs
       >>= return . fromJust . procArgs
       >>= fromFile

procArgs :: [String] -> Maybe (FilePath,TerminalType)
procArgs (fi:tt:fo:[]) = Just (fi, terminalType tt fo)
procArgs (fi:tt:[]) = Just (fi,terminalType tt "")
procArgs _ = Nothing

terminalType :: String -> FilePath -> TerminalType
terminalType "Aqua"    _ = Aqua
terminalType "Windows" _ = Windows
terminalType "X11"     _ = X11
terminalType "PS"      f = PS    f
terminalType "EPS"     f = EPS   f
terminalType "PNG"     f = PNG   f
terminalType "PDF"     f = PDF   f
terminalType "SVG"     f = SVG   f
terminalType "GIF"     f = GIF   f
terminalType "JPEG"    f = JPEG  f
terminalType "Latex"   f = Latex f

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
    title = (printf "%.3f" beta) ++ "x + " ++ (printf "%.3f" alpha)
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
