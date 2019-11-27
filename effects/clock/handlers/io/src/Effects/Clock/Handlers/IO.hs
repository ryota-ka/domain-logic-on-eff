{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Clock.Handlers.IO
    ( run
    )
where


import "base"    Data.Type.Equality             ( (:~:)(Refl) )
import "extensible" Data.Extensible             ( Eff
                                                , peelEff0
                                                )
import "time"    Data.Time                      ( getCurrentTime )

import qualified Effects.Clock                 as Clock
                                                ( AnonEff
                                                , NamedEff
                                                )
import qualified Effects.IO                    as IO
                                                ( HasEff
                                                , lift
                                                )

run :: forall effs a . IO.HasEff effs => Eff (Clock.NamedEff ': effs) a -> Eff effs a
run = peelEff0 pure interpret
  where
    interpret :: forall r . Clock.AnonEff r -> (r -> Eff effs a) -> Eff effs a
    interpret Refl k = do
        now <- IO.lift getCurrentTime
        k now
