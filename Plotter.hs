{-
 - File: Plotter.hs
 - tool to plot (x,y) data points from CSV file with a best-fit line
 - (mostly used for Chemistry)
 -}
import Text.CSV (CSV, Record, parseCSVFromFile)
import Graphics.EasyPlot
import Data.Char (isNumber)
import Data.Either.Utils (fromRight)
import Statistics.LinearRegression (linearRegression)
import Data.Vector.Unboxed (fromList)
import Data.List (transpose)
import System.Environment (getArgs)

main :: IO Bool
main = getArgs >>= return . head >>= fromFile

fromFile :: FilePath -> IO Bool
fromFile f = getCSV (f ++ ".csv")
             >>= return . (map adjust) . points
             >>= return . makeGraphs
             >>= plotGraph (f ++ ".png")

adjust :: (Double,Double) -> (Double,Double)
adjust (x,y) = (x,y)

makeGraphs :: [(Double,Double)] -> [Graph2D Double Double]
makeGraphs ps = [points_plot, best_fit_line]
  where points_plot = Data2D [Title "", Style Points] [] ps
        best_fit_line = functionPlot ps

plotGraph :: FilePath -> [Graph2D Double Double] -> IO Bool
plotGraph f gs = plotType gs
  where
    -- plotType = plot (Latex f)
    -- plotType = plot (PNG f)
    plotType = plot X11

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
