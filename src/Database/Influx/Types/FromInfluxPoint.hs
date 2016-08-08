{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Database.Influx.Types.FromInfluxPoint
    ( Result(..)
    , Parser, runParser
    , parseEither
    , FromInfluxValue (..)
    , FromInfluxPoint (..)
    , Cons (..)
    ) where

import Database.Influx.Types.Core

import Control.Monad (ap)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.HVect as HV
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- | The result of running a 'Parser'.
data Result a
    = Error String
    | Success a
    deriving (Eq, Show)

instance Functor Result where
  fmap _f (Error err) = Error err
  fmap f (Success x) = Success (f x)

instance Applicative Result where
    pure = return
    (<*>) = ap

instance Monad Result where
    return = Success
    Error err >>= _f = Error err
    Success x >>= f = f x
    fail = Error

resultToEither :: Result x -> Either String x
resultToEither r =
    case r of
      Error err -> Left err
      Success x -> Right x 

newtype Parser a
    = Parser
    { runParser :: Maybe EpochPrecision -> Result a
    }

instance Functor Parser where
    fmap f p = Parser (\ep -> f <$> runParser p ep)

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return x = Parser (\_ep -> return x)
    p >>= f =
        Parser $ \ep ->
            do x <- runParser p ep
               runParser (f x) ep
    fail err = Parser (\_ep -> fail err)

instance MonadReader (Maybe EpochPrecision) Parser where
    ask = Parser (\ep -> pure ep)
    local f x = Parser (\ep -> runParser x (f ep))

parseEither ::
       (x -> Parser a)
    -> Maybe EpochPrecision
    -> x
    -> Either String a
parseEither p ep x = resultToEither (runParser (p x) ep)

class FromInfluxValue a where
    parseInfluxValue :: Value -> Parser a

instance FromInfluxValue Value where
    parseInfluxValue = pure

instance FromInfluxValue Bool where
    parseInfluxValue val =
        case val of
          Bool b -> pure b
          _ -> fail "expected a bool"

instance FromInfluxValue Text where
    parseInfluxValue val =
        case val of
          String s -> pure s
          _ -> fail "expected a string"

instance FromInfluxValue String where
    parseInfluxValue val =
        case val of
          String s -> pure (T.unpack s)
          _ -> fail "expected a string"

instance FromInfluxValue Integer where
    parseInfluxValue val =
        case val of
          Number s ->
              case S.floatingOrInteger s :: Either Double Integer of
                Left _ -> fail "expected an integer, but got a double"
                Right i -> pure i
          Integer i -> pure i
          _ -> fail "expected an integer"

instance FromInfluxValue Int where
    parseInfluxValue val =
        case val of
          Number s ->
              case S.toBoundedInteger s of
                Nothing -> fail "expected an int, but got a double or an out-of-range integer"
                Just i -> pure i
          Integer i ->
              let intMinBound = toInteger (minBound :: Int)
                  intMaxBound = toInteger (maxBound :: Int)
              in if intMinBound <= i && i <= intMaxBound
                   then pure (fromInteger i)
                   else fail "expected an int, but got an out-of-range integer"
          _ -> fail "expected an integer"

instance FromInfluxValue a => FromInfluxValue (Maybe a) where
    parseInfluxValue val =
        case val of
          Null -> pure Nothing
          _ -> Just <$> parseInfluxValue val

{-
instance FromInfluxValue Time.UTCTime where
    parseInfluxValue val =
        case val of
          String s ->
              case Time.parseTimeM True Time.defaultTimeLocale timestampFormat (T.unpack s) of
                Nothing -> fail "could not parse string as timestamp"
                Just time -> pure time
          _ -> fail "expected a time stamp"
        where
          timestampFormat = "%Y-%m-%dT%H:%M:%SZ"
-}

class FromInfluxPoint a where
    parseInfluxPoint :: InfluxPoint -> Parser a

instance FromInfluxPoint InfluxPoint where
    parseInfluxPoint = pure

data Cons a b = Cons { car :: a, cdr :: b }

instance (FromInfluxValue a, FromInfluxPoint b) =>
    FromInfluxPoint (Cons a b) where
    parseInfluxPoint p =
        let v = point_values p
        in if V.length v >= 1
             then Cons
                 <$> parseInfluxValue (V.head v)
                 <*> parseInfluxPoint (InfluxPoint (V.tail v))
             else fail "expected a non-empty vector"

data Singleton a = Singleton a

instance FromInfluxValue a =>
    FromInfluxPoint (Singleton a) where
    parseInfluxPoint p =
        let v = point_values p
        in if V.length v >= 1
            then Singleton <$> parseInfluxValue (V.head v)
            else fail "expected a non-empty vector"

instance FromInfluxPoint () where
    parseInfluxPoint _p = pure ()

instance (FromInfluxValue a, FromInfluxValue b) => FromInfluxPoint (a, b) where
    parseInfluxPoint p =
        do Cons a (Cons b ()) <- parseInfluxPoint p
           pure (a, b)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c) => FromInfluxPoint (a, b, c) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c ())) <- parseInfluxPoint p
           pure (a, b, c)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c, FromInfluxValue d) => FromInfluxPoint (a, b, c, d) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c (Cons d ()))) <- parseInfluxPoint p
           pure (a, b, c, d)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c, FromInfluxValue d, FromInfluxValue e) => FromInfluxPoint (a, b, c, d, e) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c (Cons d (Cons e ())))) <- parseInfluxPoint p
           pure (a, b, c, d, e)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c, FromInfluxValue d, FromInfluxValue e, FromInfluxValue f) => FromInfluxPoint (a, b, c, d, e, f) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c (Cons d (Cons e (Cons f ()))))) <- parseInfluxPoint p
           pure (a, b, c, d, e, f)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c, FromInfluxValue d, FromInfluxValue e, FromInfluxValue f, FromInfluxValue g) => FromInfluxPoint (a, b, c, d, e, f, g) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c (Cons d (Cons e (Cons f (Cons g ())))))) <- parseInfluxPoint p
           pure (a, b, c, d, e, f, g)

instance (FromInfluxValue a, FromInfluxValue b, FromInfluxValue c, FromInfluxValue d, FromInfluxValue e, FromInfluxValue f, FromInfluxValue g, FromInfluxValue h) => FromInfluxPoint (a, b, c, d, e, f, g, h) where
    parseInfluxPoint p =
        do Cons a (Cons b (Cons c (Cons d (Cons e (Cons f (Cons g (Cons h ()))))))) <- parseInfluxPoint p
           pure (a, b, c, d, e, f, g, h)

instance FromInfluxPoint (HV.HVect '[]) where
    parseInfluxPoint _ = pure HV.HNil

instance (FromInfluxValue t, FromInfluxPoint (HV.HVect ts)) =>
    FromInfluxPoint (HV.HVect (t ': ts)) where
    parseInfluxPoint p =
        do Cons x xs <- parseInfluxPoint p
           pure $ x HV.:&: xs
