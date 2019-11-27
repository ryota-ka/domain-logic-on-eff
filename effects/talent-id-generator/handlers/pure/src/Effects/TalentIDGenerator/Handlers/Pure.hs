{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentIDGenerator.Handlers.Pure
    ( run
    )
where

import "base"    Data.Type.Equality             ( (:~:)(Refl) )
import "extensible" Data.Extensible             ( Eff
                                                , peelEff1
                                                )
import qualified "text" Data.Text              as T
                                                ( pack )

import qualified DomainObjects.Talent.ID       as TalentID
                                                ( shamefullyConvertFromText )
import qualified Effects.TalentIDGenerator     as TalentIDGenerator
                                                ( AnonEff
                                                , NamedEff
                                                )

run :: forall effs a . Eff (TalentIDGenerator.NamedEff ': effs) a -> Eff effs a
run effs = peelEff1 (\x _ -> pure x) interpret effs 0
  where
    interpret :: forall r . TalentIDGenerator.AnonEff r -> (r -> Integer -> Eff effs a) -> Integer -> Eff effs a
    interpret Refl k n = do
        let text     = T.pack (show n)
            talentID = TalentID.shamefullyConvertFromText text
        k talentID (n + 1)
