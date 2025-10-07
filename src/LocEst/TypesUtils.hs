{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}

module LocEst.TypesUtils where

import           LocEst.MathUtils

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as Bchs
import           GHC.Generics          (Generic)
import           Control.DeepSeq
import qualified Data.Csv              as Csv
import           Control.Applicative   (empty, (<|>))
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (catMaybes)
import qualified Codec.Serialise       as S

-- typeclasses

-- a typeclass for data types with map-like properties
class PseudoMap a b | a -> b where
    toList :: a -> [(String,b)]
    getKeys :: a -> [String]
    getValues :: a -> [b]
    lookupUnsafe :: a -> String -> b
    allSameVars :: [a] -> Bool
    filterByKey :: [String] -> a -> a

-- a typeclass for data types with ids
class Identifiable a where
    getID :: a -> String
    getIndex :: a -> Int
    setIndex :: a -> Int -> a

-- general helper functions
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- cassava helpers 

-- | A data type that wraps around bools to modify the way they are rendered in the .tsv output
-- This is specifically done to make it easily readable in R
newtype OutBool = OutBool Bool
    deriving (Eq, Show, Generic)
instance NFData OutBool
instance Csv.ToField OutBool where
    toField (OutBool True)  = "TRUE"
    toField (OutBool False) = "FALSE"

-- | A data type that wraps around Doubles to modify the way they are rendered in the .tsv output.
-- This is specifically done for the representation of inf to make it easily readable in R
newtype OutDouble = OutDouble Double
    deriving (Eq, Generic)
instance NFData OutDouble
instance Csv.ToField OutDouble where
    toField (OutDouble x)
        | x == inf    = "Inf"
        | x == (-inf) = "-Inf"
        | otherwise        = Bchs.pack $ show x
instance Show OutDouble where
    show (OutDouble x)
        | x == inf    = "Inf"
        | x == (-inf) = "-Inf"
        | otherwise        = show x

-- lookup one column by name
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- lookup optional column by name
filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField $ HM.lookup name m

-- lookup column by multiple different names and keep the first match
filterLookupMulti :: Csv.FromField a => Csv.NamedRecord -> [Bchs.ByteString] -> Csv.Parser a
filterLookupMulti m names =
    maybe empty Csv.parseField $ lookupMulti names
    where
        lookupMulti :: [Bchs.ByteString] -> Maybe Bchs.ByteString
        lookupMulti keys =
            let vals = map (`HM.lookup` m) keys
            in case catMaybes vals of
                []    -> Nothing
                (x:_) -> Just x

removeDepVarFromHeader :: String -> V.Vector Bchs.ByteString -> V.Vector Bchs.ByteString
removeDepVarFromHeader depVar =
    let s = Bchs.pack depVar
    in V.map (Bchs.intercalate "_" . filter (/= s) . Bchs.split '_')

-- | A datatype to collect additional, unpecified .csv/tsv file columns (a hashmap in cassava/Data.Csv)
newtype CsvNamedRecord = CsvNamedRecord Csv.NamedRecord deriving (Show, Eq, Generic)

getCsvNR :: CsvNamedRecord -> Csv.NamedRecord
getCsvNR (CsvNamedRecord x) = x

instance S.Serialise CsvNamedRecord
instance NFData CsvNamedRecord
instance Csv.DefaultOrdered CsvNamedRecord where
    headerOrder (CsvNamedRecord nr) =
        V.fromList $ HM.keys nr
instance Csv.ToRecord CsvNamedRecord where
    toRecord (CsvNamedRecord nr) =
        V.fromList $ HM.elems nr