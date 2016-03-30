import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split
import System.IO

main = do
    untagged <- readFile ("untagged.txt")
    let textToTag = (lines untagged)

    countriesCont <- readFile ("./data/countries/countries.txt")
    citiesCont <- readFile ("./data/cities/cities.txt")
    personsCont <- readFile ("./data/persons/persons.txt")
    let countries = (lines countriesCont)
    let cities = (lines citiesCont)
    let persons = (lines personsCont)

    let dateWords = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Monday", "Tuesday", "Wednsday", "Thursday", "Friday", "Saturday", "Sunday"]
    let suffixes = ["of", "the"]

    let result = iterLines textToTag countries cities persons dateWords []
    print textToTag
    print "-------------------"

    print result

    writeFile "tagged.txt" $ unwords result

iterLines :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterLines [] _ _ _ _ x = x
iterLines lines countries cities persons dateWords result = iterLinesCSW (checkForMoney(checkForDateWords (iterLinesTWC lines countries cities persons []) dateWords []) []) countries cities persons []


iterLinesCSW :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterLinesCSW [] _ _ _ x = x
iterLinesCSW (x:xs) countries cities persons result =
    iterLinesCSW xs countries cities persons (checkSingleWords [x] countries cities persons result)

iterLinesTWC :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterLinesTWC [] _ _ _ x = x
iterLinesTWC (x:xs) countries cities persons result =
    iterLinesTWC xs countries cities persons (iterTwoWordChunks (splitOn " " x) countries cities persons result)


iterTwoWordChunks :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterTwoWordChunks [] _ _ _ x = x
iterTwoWordChunks (x:xs) countries cities persons result
    | twoWordChunk `elem` countries = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | twoWordChunk `elem` cities = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | x `elem` persons && (head xs) `elem` persons = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ twoWordChunk ++"</ENAMEX>")])
    | otherwise = iterTwoWordChunks (xs) countries cities persons (result ++ [x])
    where twoWordChunk = (x ++ " " ++ (head xs))

checkSingleWords :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
checkSingleWords [] _ _ _ x = x
checkSingleWords (x:xs) countries cities persons result
    | x `elem` countries = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | x `elem` cities = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | x `elem` persons = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ x ++"</ENAMEX>")])
    | (length x) > 1 && (init x) `elem` countries = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 1 && (init x) `elem` cities = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 1 && (init x) `elem` persons = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` countries = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` cities = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` persons = checkSingleWords xs countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | otherwise = checkSingleWords xs countries cities persons (result ++ [x])

checkForDateWords :: [String] -> [String] -> [String] -> [String]
checkForDateWords [] _ x = x
checkForDateWords (x:xs) dateWords result
    | x `elem` ["last", "next"] && (head xs) `elem` dateWords = checkForDateWords (tail xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (head xs) ++"</TIMEX>")])
    | x `elem` dateWords = checkForDateWords xs dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++"</TIMEX>")])
    | (length x) > 1 && (init x) `elem` dateWords = checkForDateWords xs dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ (init x) ++"</TIMEX>" ++ [(last x)])])
    | (isDateNumber x) == True && (head xs) `elem` dateWords = checkForDateWords (init xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ (head xs) ++"</TIMEX>")])
    | (isDateNumber x) == True && (init (head xs)) `elem` dateWords = checkForDateWords (init xs) dateWords (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | otherwise = checkForDateWords xs dateWords (result ++ [x])


isDateNumber string = if string `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"] then True
                        else False


checkForMoney :: [String] -> [String] -> [String]
checkForMoney [] x = x
checkForMoney (x:xs) result
    | (length x) > 1 && (head x) == '$' && (init (head xs)) `elem` ["million", "thousand", "hundred"] = checkForMoney (tail xs) (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (init (head xs)) ++"</NUMEX>" ++ [(last (head xs))])])
    | (length x) > 1 && (head x) == '$' && (head xs) `elem` ["million", "thousand", "hundred"] = checkForMoney (init xs) (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (head xs) ++"</NUMEX>")])
    | (length x) > 1 && (head x) == '$' = checkForMoney xs (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++"</NUMEX>")])
    | otherwise = checkForMoney xs (result ++ [x])
