{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Clock.Handlers.Pure
    ( run
    )
where


import "extensible" Data.Extensible             ( Eff
                                                , runReaderEff
                                                )
import "time"    Data.Time                      ( UTCTime )

import qualified Effects.Clock                 as Clock
                                                ( NamedEff )

run :: forall effs a . UTCTime -> Eff (Clock.NamedEff ': effs) a -> Eff effs a
run = flip runReaderEff
