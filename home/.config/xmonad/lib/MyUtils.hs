module MyUtils
  ( showNotification,
    getCurrentDateTime,
    spawnDateTimeNotification,
    getSystemHostname,
    cycleList,
    isCommandAvailable,
    interfaceNames,
    getWifiSSID,
    getWifiSSIDString,
    findExecutableInList,
    dmenuPopup,
  )
where

import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
-- ubuntu requires: sudo apt install libghc-hostname-dev
-- arch requires: sudo pacman -S haskell-hostname
import Network.HostName (getHostName)
-- ubuntu requires: sudo apt install libghc-network-info-dev
-- arch requires: sudo pacman -S haskell-network-info
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import System.Directory
import XMonad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

showNotification :: String -> IO ()
showNotification message = io $ safeSpawn "notify-send" [message]

getCurrentDateTime :: IO String
getCurrentDateTime = runProcessWithInput "date" ["+%c"] ""

spawnDateTimeNotification :: IO ()
spawnDateTimeNotification = do
  dateTime <- getCurrentDateTime
  showNotification dateTime

cycleList :: IORef [String] -> IO String
cycleList stateRef = do
  strings <- readIORef stateRef
  let result = head strings
  writeIORef stateRef $ tail strings ++ [result]
  return result

-- | Check if a command is available in the system.
isCommandAvailable :: String -> IO Bool
isCommandAvailable cmd = do
  result <- runProcessWithInput "which" [cmd] ""
  return $ not (null result)

getSystemHostname :: IO String
getSystemHostname = getHostName

interfacesWithPrefix :: [String] -> IO [NetworkInterface]
interfacesWithPrefix prefixes = do
  interfaces <- getNetworkInterfaces
  return $ filter (\iface -> any (`isPrefixOf` name iface) prefixes) interfaces

interfaceNames :: [String] -> IO [String]
interfaceNames prefixes = do
  interfaces <- interfacesWithPrefix prefixes
  return $ map name interfaces

getWifiSSID :: IO (Maybe String)
getWifiSSID = do
  foundExecutable <- findExecutable "iwgetid"
  case foundExecutable of
    Just _ -> do
      result <- runProcessWithInput "iwgetid" ["-r"] ""
      return $ if null result then Nothing else Just (init result)
    Nothing -> return Nothing

getWifiSSIDString :: IO String
getWifiSSIDString = fromMaybe "" <$> getWifiSSID

findExecutableInList :: [String] -> IO (Maybe String)
findExecutableInList = go
  where
    go [] = return Nothing
    go (exe : rest) = do
      mPath <- findExecutable exe
      case mPath of
        Just _ -> return (Just exe)
        Nothing -> go rest

dmenuPopup :: String -> [String] -> IO String
dmenuPopup prompt options = do
  let input = unlines options
  output <- runProcessWithInput "rofi" ["-dmenu", "-i", "-p", prompt] input
  return $ init output
