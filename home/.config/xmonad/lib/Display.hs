module Display (
showNotification,
toggleResolution,
getPrimaryDisplay,
) where
import Control.Monad.State
import Data.IORef
import Data.List (isPrefixOf, find,isInfixOf, elemIndex,tails)
import Data.Maybe (fromMaybe, mapMaybe,listToMaybe)

-- sudo apt-get install libghc-regex-posix-dev
import Text.Regex.Posix ((=~))
import System.Directory
import System.Process (callCommand, readProcess)
import qualified Data.Map        as M
showNotification :: String -> IO ()
showNotification = putStrLn 
resolutionsMap :: M.Map String [String]
resolutionsMap = M.fromList [
--    ("eDP-1", ["1920x1080"]),
    ("DP-3" , ["3440x1440", "1920x1080"])
    -- Add more displays and their resolutions here
    ]-- Function to get the primary display
getPrimaryDisplay :: IO String
getPrimaryDisplay = do
    output <- readProcess "xrandr" ["--query"] ""
    let primaryLine = head $ filter (isInfixOf " primary") (lines output)
    return $ head $ words primaryLine

splitDisplayLine :: String -> String -> Maybe (String, String)
splitDisplayLine sep str =
    let (a, b) = break (isPrefixOf sep) (tails str)
    in case b of
        [] -> Nothing
        (x:_) | sep `isPrefixOf` x -> Just (take (length str - length x) str, drop (length sep) x)
              | otherwise -> Nothing

splitAndMatch :: String -> String -> Maybe String
splitAndMatch regex str =
    let matches = str =~ regex :: [[String]]
    in listToMaybe (concat matches)

splitAndMatch2 :: String -> String -> [String]
splitAndMatch2 regex str =
    let wordsList = words str
    in filter (=~ regex) wordsList

matchGroups :: String -> Int -> String -> Maybe String
matchGroups regex groupIndex word = let matches = word =~ regex :: [[String]]
                   in if null matches || length (head matches) <= groupIndex
                      then Nothing
                      else Just ((head matches) !! groupIndex)

splitAndMatchWithGroup :: String -> Int -> String -> [[String]]
splitAndMatchWithGroup regex groupIndex str = do
    let wordsList = words str
    return $ mapMaybe (matchGroups regex groupIndex) wordsList

invocation :: String -> [[String]]
invocation = splitAndMatchWithGroup "([0-9]+x[0-9]+)\\+[0-9]+\\+[0-9]+" 1
-- Usage example:
-- splitAndMatchWithGroup "([0-9]+)" 1 "123 abc 456 def 789"
-- This will return ["123", "456", "789"]

getCurrentResolution2 :: IO (M.Map String String)
getCurrentResolution2 = do
    output <- readProcess "xrandr" ["--current", "--verbose"] ""
    let displayLines = filter (not . isPrefixOf " ") (lines output)

    let linesByDisplay = M.fromList $ mapMaybe (splitDisplayLine " connected ") displayLines
    putStrLn "Lines by Display:"
    mapM_ print (M.toList linesByDisplay)

    let updatedLinesByDisplay = M.mapMaybe (splitAndMatch "\\d+x\\d+") linesByDisplay
    putStrLn "Updated Lines by Display:"
    mapM_ print (M.toList updatedLinesByDisplay)

    return updatedLinesByDisplay

getCurrentResolution :: IO (M.Map String String)
getCurrentResolution = do
    output <- readProcess "xrandr" ["--current", "--verbose"] ""
    let displayLines = filter (isInfixOf " connected") (lines output)
    let currentResolutions = mapMaybe (splitDisplayLine " connected") displayLines
    return $ M.fromList currentResolutions

-- Function to toggle between the resolutions
toggleResolution :: M.Map String [String] -> IO  ()
toggleResolution resolutionsMap = do
    display <- getPrimaryDisplay
    showNotification ("Display " ++ display )
    case M.lookup display resolutionsMap of
        Nothing -> do
            showNotification ("Display " ++ display ++ " not found in the map")
        Just resolutions -> do
            showNotification ("Display " ++ display ++ " found in the map")
            currentResolution <- fmap (head . words) $ readProcess "xrandr" ["--current", "--verbose"] ""
            currentResolution <-  readProcess "xrandr" ["--current", "--verbose"] ""
            showNotification  $ "currentResolution: " ++ currentResolution
            let currentIndex = fromMaybe 0 (elemIndex currentResolution resolutions)
            showNotification $ "currentIndex: " ++show currentIndex
            let nextIndex = (currentIndex + 1) `mod` length resolutions
            let newResolution = resolutions !! nextIndex
            callCommand $ "xrandr --output " ++ display ++ " --mode " ++ newResolution

