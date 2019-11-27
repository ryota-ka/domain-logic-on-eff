{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentIDGenerator.Handlers.UUIDV4
    ( run
    )
where

import "base"    Data.Type.Equality             ( (:~:)(Refl) )
import "extensible" Data.Extensible             ( Eff
                                                , peelEff0
                                                )
import qualified "uuid" Data.UUID              as UUID
                                                ( toText )
import qualified "uuid" Data.UUID.V4           as UUID
                                                ( nextRandom )

import qualified DomainObjects.Talent.ID       as TalentID
                                                ( shamefullyConvertFromText )
import qualified Effects.IO                    as IO
                                                ( HasEff
                                                , lift
                                                )
import qualified Effects.TalentIDGenerator     as TalentIDGenerator
                                                ( AnonEff
                                                , NamedEff
                                                )

run :: forall effs a . IO.HasEff effs => Eff (TalentIDGenerator.NamedEff ': effs) a -> Eff effs a
run = peelEff0 pure interpret
  where
    interpret :: forall r . TalentIDGenerator.AnonEff r -> (r -> Eff effs a) -> Eff effs a
    interpret Refl k = do
        uuid <- IO.lift UUID.nextRandom
        let text     = UUID.toText uuid
            talentID = TalentID.shamefullyConvertFromText text
        k talentID
