{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module DomainObjects.Talent.ID
  ( ID
  , shamefullyConvertFromText
  )
where

import "text"    Data.Text                      ( Text )

newtype ID
    = ID Text
    deriving newtype Ord
    deriving newtype Eq

shamefullyConvertFromText :: Text -> ID
shamefullyConvertFromText = ID
