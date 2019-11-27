{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentRepository
    ( AnonEff
    , find
    , HasEff
    , NamedEff
    , store
    , TalentRepository(..)
    )
where


import "base"    Data.Proxy                     ( Proxy(Proxy) )
import "extensible" Data.Extensible             ( type (>:)
                                                , Eff
                                                , liftEff
                                                , Lookup
                                                )

import           DomainObjects.Talent           ( Talent )
import qualified DomainObjects.Talent.ID       as Talent
                                                ( ID )

data TalentRepository a where
    Find ::Talent.ID -> TalentRepository (Maybe Talent)
    Store ::Talent -> TalentRepository ()

type EffName = "TalentRepository"
type AnonEff = TalentRepository
type NamedEff = EffName >: AnonEff
type HasEff effs = Lookup effs EffName TalentRepository

proxy :: Proxy EffName
proxy = Proxy

lift :: forall effs a . HasEff effs => TalentRepository a -> Eff effs a
lift = liftEff proxy

find :: forall effs . HasEff effs => Talent.ID -> Eff effs (Maybe Talent)
find talentID = lift (Find talentID)

store :: forall effs . HasEff effs => Talent -> Eff effs ()
store talent = lift (Store talent)
