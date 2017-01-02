module ParseCSV where
import Text.ParserCombinators.Parsec
import Data.CSV

main = do print (parse csvFile "(unknown)" "line1\nline2\nline3\n")

