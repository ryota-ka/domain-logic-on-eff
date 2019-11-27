{-# LANGUAGE PackageImports #-}

module DomainObjects.Talent.AddedAt
    ( AddedAt
    , fromUTCTime
    )
where

import "time"    Data.Time                      ( UTCTime )

newtype AddedAt
    = AddedAt UTCTime

fromUTCTime :: UTCTime -> AddedAt
fromUTCTime = AddedAt
