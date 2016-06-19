module Database.Influx.Internal.Helpers
    ( urlAppend
    ) where

urlAppend :: String -> String -> String
urlAppend base path = base' ++ "/" ++ path'
  where base' = if last base == '/' then init base else base
        path' = if head path == '/' then tail path else path