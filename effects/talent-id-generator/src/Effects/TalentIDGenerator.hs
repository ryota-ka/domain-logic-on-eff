{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentIDGenerator
    ( AnonEff
    , HasEff
    , NamedEff
    , next
    )
where


import "base"    Data.Proxy                     ( Proxy(Proxy) )
import "extensible" Data.Extensible             ( type (>:)
                                                , askEff
                                                , Eff
                                                , Lookup
                                                , ReaderEff
                                                )

import qualified DomainObjects.Talent.ID       as Talent
                                                ( ID )

type EffName = "TalentIDGenerator"
type AnonEff = ReaderEff Talent.ID
type NamedEff = EffName >: AnonEff
type HasEff effs = Lookup effs EffName AnonEff

proxy :: Proxy EffName
proxy = Proxy

next :: forall effs . HasEff effs => Eff effs Talent.ID
next = askEff proxy
