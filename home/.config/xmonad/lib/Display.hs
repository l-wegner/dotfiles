module Display
  ( toggleResolution,
    getPrimaryDisplay,
    switchResolution,
  )
where

import Control.Exception (catch)
import Control.Monad.State
import Data.IORef
import Data.List (elemIndex, find, isInfixOf, isPrefixOf, tails)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import MyUtils (dmenuPopup, showNotification)
-- import System.Directory
import System.Process (callCommand, readProcess)
-- sudo apt-get install libghc-regex-posix-dev
import Text.Regex.Posix ((=~))
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

-- showNotification :: String -> IO ()
-- showNotification = putStrLn

-- Function to get the primary display
getPrimaryDisplay :: IO String
getPrimaryDisplay = do
  output <- runProcessWithInput "xrandr" ["--query"] ""
  let primaries = filter (isInfixOf " primary ") (lines output)
  case primaries of
    [] -> do
      showNotification "No primary display found"
      return ""
    (primaryLine : _) -> do
      return $ head $ words primaryLine

splitDisplayLine :: String -> String -> Maybe (String, String)
splitDisplayLine sep str =
  let (a, b) = break (isPrefixOf sep) (tails str)
   in case b of
        [] -> Nothing
        (x : _)
          | sep `isPrefixOf` x -> Just (take (length str - length x) str, drop (length sep) x)
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
matchGroups regex groupIndex word =
  let matches = word =~ regex :: [[String]]
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
toggleResolution :: M.Map String [String] -> IO ()
toggleResolution resolutionsMap = do
  showNotification "Toggling resolution"
  display <- getPrimaryDisplay
  case M.lookup display resolutionsMap of
    Nothing -> do
      showNotification $ "Display " ++ display ++ " not found in the map"
    Just resolutions -> do
      xrandrOutput <- runProcessWithInput "xrandr" ["--current"] ""
      let primaryLine = head $ filter (isInfixOf (display ++ " connected")) (lines xrandrOutput)
      showNotification $ "primaryLine: " ++ primaryLine
      let resWords = filter (=~ "^[0-9]+x[0-9]+\\+.*") (words primaryLine)
          currentResolution =
            if null resWords
              then ""
              else takeWhile (/= '+') (head resWords)
      showNotification $ "currentResolution: " ++ currentResolution
      case elemIndex currentResolution resolutions of
        Nothing -> showNotification $ "Current resolution(" ++ currentResolution ++ ") not found in list"
        Just currentIndex -> do
          -- proceed as before
          let nextIndex = (currentIndex + 1) `mod` length resolutions
          let newResolution = resolutions !! nextIndex
          callCommand $ "xrandr --output " ++ display ++ " --mode " ++ newResolution

selectDisplay :: IO String
selectDisplay = do
  output <- runProcessWithInput "xrandr" ["--query"] ""
  let displayLines = filter (isInfixOf " connected") (lines output)
  let displayNames = map (head . words) displayLines
  dmenuPopup "Select display:" displayNames

getAvailableResolutions :: String -> IO [String]
getAvailableResolutions display = do
  output <- runProcessWithInput "xrandr" ["--query"] ""
  let ls = lines output
      displayLineIndex = case filter (\(i, l) -> (display ++ " connected") `isInfixOf` l) (zip [0 ..] ls) of
        ((i, _) : _) -> i
        _ -> -1
      resolutions =
        if displayLineIndex >= 0
          then
            takeWhile
              (\l -> not (null l) && (head l == ' ' || head l == '\t'))
              (drop (displayLineIndex + 1) ls)
          else []
      availableRes = mapMaybe (splitAndMatch "^[ ]*([0-9]+x[0-9]+)") resolutions
  return availableRes

selectResolution :: String -> IO String
selectResolution display = do
  resolutions <- getAvailableResolutions display
  dmenuPopup ("Select resolution for " ++ display ++ ":") resolutions

switchResolution :: IO ()
switchResolution = do
  selection <- selectDisplay
  showNotification $ "You selected: " ++ selection
  resolution <- selectResolution selection
  showNotification $ "You selected resolution: " ++ resolution
  callCommand $ "xrandr --output " ++ selection ++ " --mode " ++ resolution
