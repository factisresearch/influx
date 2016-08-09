{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functions related to query serialization.
module Database.Influx.Internal.Helpers
    ( urlAppend
    , credsToQueryString
    , epochToBytestring
    , queryParamsToQueryString
    , serializeInfluxData
    , writeParamsToQueryString
    )
where

import Database.Influx.Types.Core

import Control.Arrow (second)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Safely appends two URL components. Prevents two slashes from clashing.
urlAppend :: String -> String -> String
urlAppend base path = base' ++ "/" ++ path'
    where
      base' = if last base == '/' then init base else base
      path' = if head path == '/' then tail path else path

-- | Converts credentials to a string that can be used in a valid query.
credsToQueryString :: Credentials -> [(B.ByteString, Maybe B.ByteString)]
credsToQueryString creds =
    fmap (second Just) $
    [ ("u", T.encodeUtf8 (creds_user creds))
    , ("p", T.encodeUtf8 (creds_password creds))
    ]

-- | Renders a time unit (epoch precision) into the commonly known symbols. (h/m/s/ms/us/ns)
epochToBytestring :: EpochPrecision -> B.ByteString
epochToBytestring epoch =
    case epoch of
      Hours -> "h"
      Minutes -> "m"
      Seconds -> "s"
      Milliseconds -> "ms"
      Microseconds -> "us"
      Nanoseconds -> "ns"

-- | Renders a type-safe 'QueryParams' value into a key-value pair for a valid query.
queryParamsToQueryString :: QueryParams -> [(B.ByteString, Maybe B.ByteString)]
queryParamsToQueryString opts =
    fmap (second Just) $
    catMaybes
    [ (,) "chunk_size" . T.encodeUtf8 . T.pack . show <$> qp_chunkSize opts
    , (,) "epoch" . epochToBytestring <$> qp_epoch opts
    , (,) "rp" . T.encodeUtf8 <$> qp_retentionPolicy opts
    , (,) "db" . T.encodeUtf8 <$> qp_database opts
    ]

serializeValue :: Value -> Maybe Text
serializeValue v =
    case v of
      Number n -> Just $ T.pack $ show n
      Integer i -> Just $ T.pack (show i) <> "i"
      String s -> Just $ T.pack $ show s
      Bool b ->
        Just $ if b then "true" else "false"
      Null -> Nothing

-- | Converts actual InfluxDB data into valid
-- <https://docs.influxdata.com/influxdb/v0.13//write_protocols/write_syntax/#line-protocol
-- line protocol> data.
serializeInfluxData :: InfluxData -> Text
serializeInfluxData d =
    T.intercalate "," (escape (data_measurement d) : map serializeTag (data_tags d)) <> " " <>
    T.intercalate "," (mapMaybe serializeField (data_fields d)) <>
    maybe "" (\t -> " " <> serializeTimeStamp t) (data_timestamp d)
    where
      serializeTag (k, v) = escape k <> "=" <> escape v
      serializeField (k, v) = ((escape k <> "=") <>) <$> serializeValue v
      serializeTimeStamp t = T.pack $ show $ unTimeStamp t
      escape = T.replace "," "\\," . T.replace " " "\\ "

-- | Renders a type-safe 'WriteParams' value into a key-value pair for a valid query.
writeParamsToQueryString :: WriteParams -> [(B.ByteString, Maybe B.ByteString)]
writeParamsToQueryString opts =
    fmap (second Just) $
    catMaybes
    [ (,) "precision" . epochToBytestring <$> wp_precision opts
    , (,) "rp" . T.encodeUtf8 <$> wp_retentionPolicy opts
    ]
