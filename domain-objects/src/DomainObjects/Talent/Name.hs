{-# LANGUAGE PackageImports #-}

module DomainObjects.Talent.Name
  ( Name
  , shamefullyConvertFromText
  )
where

import "text"    Data.Text                      ( Text )

-- | タレントの名前を表す
-- e.g. @採用 太郎@
newtype Name
    = Name Text

shamefullyConvertFromText :: Text -> Name
shamefullyConvertFromText = Name
