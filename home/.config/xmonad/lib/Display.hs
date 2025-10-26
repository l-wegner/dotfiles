module Display
  ( toggleResolution,
    getPrimaryDisplay,
    switchResolution,
  )
where

import Data.List (elemIndex, isInfixOf)
import Data.Map qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import MyUtils (dmenuPopup, showNotification)
-- import System.Directory
import System.Process (callCommand)
-- sudo apt-get install libghc-regex-posix-dev
import Text.Regex.Posix ((=~))
import XMonad.Util.Run (runProcessWithInput)

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

splitAndMatch :: String -> String -> Maybe String
splitAndMatch regex str =
  let matches = str =~ regex :: [[String]]
   in listToMaybe (concat matches)

getCurrentResolution :: IO (M.Map String String)
getCurrentResolution = do
  output <- runProcessWithInput "xrandr" ["--current"] ""
  let displayLines = filter (isInfixOf " connected") (lines output)
      parseRes line =
        let ws = words line
            display = head ws
            resWords = filter (=~ "^[0-9]+x[0-9]+\\+.*") ws
         in case resWords of
              (r : _) -> Just (display, takeWhile (/= '+') r)
              _ -> Nothing
      currentResolutions = mapMaybe parseRes displayLines
  return $ M.fromList currentResolutions

-- Function to toggle between the resolutions
toggleResolution :: M.Map String [String] -> IO ()
toggleResolution resolutionsMap = do
  display <- getPrimaryDisplay
  currentResMap <- getCurrentResolution
  case M.lookup display resolutionsMap of
    Nothing -> showNotification $ "Display " ++ display ++ " not found in the map"
    Just resolutions -> do
      let currentResolution = M.findWithDefault "" display currentResMap
      case elemIndex currentResolution resolutions of
        Nothing -> showNotification $ "Current resolution(" ++ currentResolution ++ ") not found in list for display " ++ display
        Just currentIndex -> do
          let nextIndex = (currentIndex + 1) `mod` length resolutions
              newResolution = resolutions !! nextIndex
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
  resolution <- selectResolution selection
  callCommand $ "xrandr --output " ++ selection ++ " --mode " ++ resolution
