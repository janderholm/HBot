{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Int
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as S

import GHC.Generics
import Control.Applicative

import Network.HTTP.Conduit


---------------------------------- Api calls ----------------------------------
get_server_time:: IO (Either String (KrakenResponse KrakenTimestamp))
get_server_time = eitherDecode <$> call where
  call = simpleHttp "https://api.kraken.com/0/public/Time"

get_assets:: IO (Either String (KrakenResponse [KrakenCurrency]))
get_assets = eitherDecode <$> call where
    call = simpleHttp "https://api.kraken.com/0/public/Assets"


------------------------------- Data Structures -------------------------------
data KrakenResponse a = KrakenResponse
  { error :: [T.Text]
  , result :: a
  } deriving (Show, Generic)

data KrakenTimestamp = KrakenTimestamp
  { unixtime :: UTCTime
  , rfc1123  :: T.Text
  } deriving (Show, Generic)

data KrakenCurrency = KrakenCurrency
  { name :: T.Text
  , decimals :: Int
  , display_decimals :: Int
  } deriving (Show, Generic)


------------------------------ Parsing instances ------------------------------
instance FromJSON (KrakenResponse [KrakenCurrency])
instance FromJSON (KrakenResponse KrakenTimestamp)

instance FromJSON KrakenTimestamp where
  parseJSON (Object v) = do
    -- Unix time is a 64 bit integer.
    posix_t <- (parseJSON =<< (v .: "unixtime")) :: AT.Parser Int64
    rfc1123 <- parseJSON =<< (v .: "rfc1123")
    return $ KrakenTimestamp (posixSecondsToUTCTime . realToFrac $ posix_t) rfc1123

instance FromJSON KrakenCurrency where
  parseJSON (Object v) =
    KrakenCurrency <$> v .: "altname"
                   <*> v .: "decimals"
                   <*> v .: "display_decimals"

instance FromJSON [KrakenCurrency] where
  parseJSON (Object v) = sequence $ map parseJSON (S.elems v) :: AT.Parser [KrakenCurrency]

-------------------------------------------------------------------------------


-- | 'main' runs the main program
main :: IO ()
main = do
  d <- get_server_time
  case d of
    Left err   -> putStrLn $ "Error: " ++ err
    Right resp -> print resp

  as <- get_assets
  case as of
    Left err   -> putStrLn $ "Error: " ++ err
    Right resp -> print resp
