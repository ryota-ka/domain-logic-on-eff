{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Effects.IO
    ( AnonEff
    , EffName
    , HasEff
    , NamedEff
    , lift
    )
where


import "base"    Data.Proxy                     ( Proxy(Proxy) )
import "extensible" Data.Extensible             ( type (>:)
                                                , Eff
                                                , liftEff
                                                , Lookup
                                                )

type EffName = "IO"
type AnonEff = IO
type NamedEff = EffName >: AnonEff
type HasEff effs = Lookup effs EffName IO

proxy :: Proxy EffName
proxy = Proxy

lift :: forall effs a . HasEff effs => IO a -> Eff effs a
lift = liftEff proxy
