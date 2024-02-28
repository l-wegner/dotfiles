module MyUtils (
showNotification,
getCurrentDateTime,
spawnDateTimeNotification,
cycleList,
isCommandAvailable,
interfaceNames,
getWifiSSID,
getWifiSSIDString,
findExecutableInList
) where
import Control.Monad.State
import Data.IORef
import Data.List (isPrefixOf, find)
import Data.Maybe (fromMaybe)
-- ubuntu requires: sudo apt install libghc-hostname-dev
-- arch requires: sudo pacman -S haskell-hostname
import Network.HostName (getHostName)
-- ubuntu requires: sudo apt install libghc-network-info-dev
-- arch requires: sudo pacman -S haskell-network-info
import Network.Info (getNetworkInterfaces, ipv4, NetworkInterface(..))

import System.Directory
import XMonad
import XMonad.Util.Run (safeSpawn,runProcessWithInput)

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


interfacesWithPrefix ::  [String] -> IO [NetworkInterface]
interfacesWithPrefix prefixes = do
  interfaces <- getNetworkInterfaces
  return $ filter (\iface -> any (\prefix -> isPrefixOf prefix (name iface)) prefixes) interfaces


interfaceNames ::  [String] -> IO [String]
interfaceNames prefixes = do
  interfaces <- interfacesWithPrefix prefixes
  return $ map name interfaces


getWifiSSID :: IO (Maybe String)
--getWifiSSID = do
--    result <-  runProcessWithInput "iwgetid" ["-r"] ""
--    return $ if null result then Nothing else Just (init result)
getWifiSSID = do
    foundExecutable <- findExecutable "iwgetid"
    case foundExecutable of
        Just _  -> do
            result <- runProcessWithInput "iwgetid" ["-r"] ""
            return $ if null result then Nothing else Just (init result)
        Nothing -> return $ Nothing

getWifiSSIDString :: IO (String)
getWifiSSIDString = fromMaybe "" <$> getWifiSSID

findExecutableInList :: [String] -> IO (Maybe String)
findExecutableInList executables = go executables
  where
    go [] = return Nothing
    go (exe:rest) = do
        mPath <- findExecutable exe
        case mPath of
            Just path -> return (Just exe)
            Nothing    -> go rest

myPopup :: String -> IO ()
myPopup msg = spawn $ "echo '" ++ msg ++ "' | dzen2 -p 2 -h 30 -w 200 -x 500 -y 500 -fn 'xft:Monospace-12' -bg '#rrggbb' -fg '#rrggbb'"

test :: IO ()
test = do
    interfaces <- interfaceNames ["wl"]
    ssid <-  getWifiSSIDString
    hostname<-getSystemHostname
    showNotification ("hostname: " ++ hostname)
    compositor <- findExecutableInList [ "xoxo", "picom", "compton" ]
    showNotification ("compositor: " ++ maybe "not found" id compositor)

--    let myList = ["first", "second", "third"]
--    currentIndexRef <- newIORef myList
--
--    result1 <- cycleList currentIndexRef
--    putStrLn result1
--    result2 <- cycleList currentIndexRef
--    putStrLn result2
--    result3 <- cycleList currentIndexRef
--    putStrLn result3
--    result4 <- cycleList currentIndexRef
--    putStrLn result4