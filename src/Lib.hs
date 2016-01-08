{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( downloadTo
  ) where

import Control.Lens ( (^.), (^?) )
import Control.Monad ( unless, when )
import Data.Aeson ( Object, Value )
import Data.Aeson.Lens ( key, nonNull, _Array, _String )
import Data.ByteString.Lazy as ByteString ( ByteString, hPut, pack, take)
import Data.List ( isPrefixOf )
import Data.Text as Text ( unpack )
import Data.Vector as Vector ( toList )
import FileUtil ( absolutize )
import Literal ( literalEnv )
import Network.Wreq ( asJSON, get, Response, responseBody, responseStatus, statusCode, responseHeaders )
import System.Directory ( getDirectoryContents )
import System.FilePath ( takeBaseName, takeExtension, (</>), (<.>) )
import System.IO ( hClose, IOMode ( WriteMode ), openFile )

endpoint    = "https://api.pinterest.com/v1"
accessToken = [literalEnv|PINTEREST_ACCESS_TOKEN|]

data Pin = Pin { pinId :: String, pinImageUrl :: String } deriving (Show)

downloadTo :: String -> IO ()
downloadTo dest =
  do pins <- getPins
     absDest <- absolutize dest
     savePins absDest pins

getPins :: IO [Pin]
getPins =
  let firstUrl = endpoint ++ "/boards/nesachirou/%E3%82%82%E3%82%82%E3%82%93%E3%81%8C-momonga/pins/?access_token=" ++ accessToken ++ "&fields=id,image&limit=100"
  in getPins_ firstUrl []

getPins_ :: String -> [Pin] -> IO [Pin]
getPins_ url pins =
  do r <- asJSON =<< get url :: IO (Response Value)
     print (r ^. responseHeaders)
     unless (r ^. responseStatus . statusCode == 200) $
       fail $ show (r ^. responseStatus)
     let newPins = fmap makePin $ Vector.toList $ r ^. responseBody . key "data" . _Array
         next    = r ^? responseBody . key "page" . key "next" . _String
     case next of
       Nothing      -> return (pins ++ newPins)
       Just nextUrl -> getPins_ (unpack nextUrl) (pins ++ newPins)
  where
    makePin :: Value -> Pin
    makePin item = Pin { pinId       = unpack $ item ^. key "id" . _String
                       , pinImageUrl = unpack $ item ^. key "image" . key "original" . key "url" . _String
                       }

savePins :: String -> [Pin] -> IO ()
savePins _ []                = return ()
savePins dest (pin:restPins) =
  do isSaved <- isPinSaved dest pin
     unless isSaved $ savePin dest pin
     savePins dest restPins

savePin :: String -> Pin -> IO ()
savePin dest Pin { pinId = pinId, pinImageUrl = pinImageUrl } =
  do print Pin { pinId = pinId, pinImageUrl = pinImageUrl }
     r <- get pinImageUrl :: IO (Response ByteString)
     print (r ^. responseHeaders)
     when (r ^. responseStatus . statusCode == 200) $
       do file <- openFile (dest </> pinId <.> extensionOf (r ^. responseBody) pinImageUrl) WriteMode
          hPut file (r ^. responseBody)
          hClose file
  where
    extensionOf :: ByteString -> String -> String
    extensionOf content path
      | isJpeg content = ".jpg"
      | isPng content  = ".png"
      | otherwise      = takeExtension path

isPinSaved :: String -> Pin -> IO Bool
isPinSaved dest Pin { pinId = pinId } =
  do paths <- getDirectoryContents dest
     return $ any (isPrefixOf pinId) $ fmap takeBaseName paths

isJpeg :: ByteString -> Bool
isJpeg content = ByteString.take 11 content == pack [0xff, 0xd8, 0xff, 0xe0, 0x00, 0x10, 0x4a, 0x46, 0x49, 0x46, 0x00]

isPng :: ByteString -> Bool
isPng content = ByteString.take 8 content == pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
