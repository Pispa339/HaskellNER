import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split
import Data.List
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

    let result = tagLines textToTag countries cities persons dateWords []
    print textToTag
    print "-------------------"

    print result

    writeFile "result.txt" $ unlines result

    tagged <- readFile ("tagged.txt")
    let taggedLines = (lines tagged)
    myTagged <- readFile ("result.txt")
    let myTaggedLines = (lines myTagged)

    let tagsFromTagged = getTags taggedLines []
    let tagsFromMyTagged = getTags myTaggedLines []

    print ("_______")
    print tagsFromTagged
    print ("*******")
    print tagsFromMyTagged

getTags :: [String] -> [String] -> [String]
getTags [] x = x
getTags (x:xs) confirmedTags = (getTags xs (confirmedTags ++ (getTagsFromLine (splitOn " " x) [])))

getTagsFromLine :: [String] -> [String] -> [String]
getTagsFromLine [] x = x
getTagsFromLine (x:xs) confirmedTags
    | (x == "<ENAMEX" || x == "<TIMEX" || x == "<NUMEX")  = (getTagsFromLine (drop (length (splitOn " " tagContents)) (x:xs)) (confirmedTags ++ [tagContents]))
    | otherwise = getTagsFromLine (xs) confirmedTags
    where tagContents = (getTagContents (x:xs) "")

getTagContents :: [String] -> String -> String
getTagContents [] x = x
getTagContents (x:xs) tagContent
    | "</ENAMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</TIMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</NUMEX>" `isInfixOf` x = (tagContent ++ x)
    | otherwise = getTagContents xs (tagContent ++ x ++ " ")

tagLines :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> [String]
tagLines [] _ _ _ _ x = x
tagLines (x:xs) countries cities persons dateWords result =
    tagLines xs countries cities persons dateWords (result ++ [(asList (checkSingleWords (checkForMoney(checkForDateWords (iterTwoWordChunks (checkForOrganizations (splitOn " " x) []) countries cities persons []) dateWords []) []) countries cities persons []))])

iterTwoWordChunks :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
iterTwoWordChunks [] _ _ _ x = x
iterTwoWordChunks (x:xs) countries cities persons result
    | (null xs) == False && twoWordChunk `elem` countries = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && twoWordChunk `elem` cities = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && x `elem` persons && (head xs) `elem` persons = iterTwoWordChunks (tail xs) countries cities persons (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ twoWordChunk ++"</ENAMEX>")])
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

checkForOrganizations :: [String] -> [String] -> [String]
checkForOrganizations [] x = x
checkForOrganizations (x:xs) result
    | (length organizationList) > 2 && (head organizationList) `elem` ["of", "Of", "The", "the"] && (last (last organizationList)) `elem` ['.', ','] = checkForOrganizations (drop (length organizationList) (x:xs)) (result ++ [(head organizationList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (asList (init (tail organizationList))) ++ " " ++ (init (last organizationList)) ++ "</ENAMEX>" ++ [(last (last organizationList))])])
    | (length organizationList) > 2 && (head organizationList) `elem` ["of", "Of", "The", "the"] = checkForOrganizations (drop (length organizationList) (x:xs)) (result ++ [(head organizationList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (asList (tail organizationList)) ++ "</ENAMEX>")])
    | otherwise = checkForOrganizations xs (result ++ [x])
    where organizationList = (formOrganizationList (x:xs) [])

formOrganizationList words org = if (null words) == False
                                    then if (length (head words)) > 1 && ((head (head words)) `elem` ['A' .. 'Z'] || (head words) `elem` ["of", "the", "Inc"])
                                        then formOrganizationList (tail words) (org ++ [(head words)])
                                        else if (length org) < 2
                                            then []
                                            else org
                                        else if (length org) < 2
                                            then []
                                            else org

asList :: [String] -> String
asList ss = (intercalate " " ss)
