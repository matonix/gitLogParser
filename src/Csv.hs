{-# LANGUAGE DeriveGeneric #-}

module Csv
  ( readCSV
  , writeCSV
  , LogCsv(..)
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.Vector                (Vector)
import           GHC.Generics               (Generic)

data LogCsv = LogCsv
  { reposC   :: String
  , commitC  :: String
  , dateC    :: String
  , issueIdC :: String
  } deriving (Show, Generic)

instance FromRecord LogCsv
instance ToRecord LogCsv

readCSV :: FromRecord a => FilePath -> IO (Either String (Vector a))
readCSV csvFile = decode NoHeader . BL.fromStrict <$> BS.readFile csvFile

writeCSV :: ToRecord a => FilePath -> [a] -> IO ()
writeCSV outPath csv = BL.writeFile outPath $ encode csv
