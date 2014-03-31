-- file: qmbl2csv.hs

import Data.Functor         ((<$>))
import Data.List            (transpose, find)
import Data.Maybe           (fromJust)
import System.Environment   (getArgs)
import System.IO            (readFile, writeFile)
import Text.CSV             (printCSV, Field, CSV)
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Proc  (onlyElems, filterChildName, filterElementsName, strContent)
import Text.XML.Light.Types (qName, elName, Content, Element)

type QMBL = [Content]

main :: IO ()
main = inFile >>= readFile
       >>= return . filter (\c -> (c /= '\"')) . printCSV . qmblToCSV . parseXML
       >>= (\s -> (outFile >>= (\f -> writeFile f s)))
  where inFile  = head <$> getArgs
        outFile = last <$> getArgs

qmblToCSV :: QMBL -> CSV
qmblToCSV qmbl = filter (all (not . null))
                 $ map (map (filter (\c -> (c /= '\"'))))
                 $ transpose $ columns qmbl

columns :: QMBL -> [[Field]]
columns = map (\c -> (columnName c) : (columnPoints c))
          . dataColumns . document

columnPoints :: Element -> [Field]
columnPoints = map show . lines . strContent . fromJust
               . filterChildName (\n -> (qName n) == "ColumnCells")

columnName :: Element -> Field
columnName = show . strContent . fromJust
             . filterChildName (\n -> (qName n) == "ColumnMatchTag")

dataColumns :: Element -> [Element]
dataColumns = filterElementsName (\n -> (qName n) == "DataColumn")

document :: QMBL -> Element
document = fromJust . find (\e -> (qName $ elName e) == "Document") . onlyElems
