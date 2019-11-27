{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Clock
    ( AnonEff
    , ask
    , HasEff
    , NamedEff
    )
where


import "base"    Data.Proxy                     ( Proxy(Proxy) )
import "extensible" Data.Extensible             ( type (>:)
                                                , askEff
                                                , Eff
                                                , Lookup
                                                , ReaderEff
                                                )
import "time"    Data.Time                      ( UTCTime )

type EffName = "Clock"
type AnonEff = ReaderEff UTCTime
type NamedEff = EffName >: AnonEff
type HasEff effs = Lookup effs EffName (ReaderEff UTCTime)

proxy :: Proxy EffName
proxy = Proxy

ask :: forall effs . HasEff effs => Eff effs UTCTime
ask = askEff proxy
