{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentRepository.Handlers.IORef
    ( run
    )
where

import           Prelude                 hiding ( id )

import "base"    Data.IORef                     ( atomicModifyIORef'
                                                , IORef
                                                , readIORef
                                                )
import "containers" Data.Map.Strict             ( Map )
import "containers" Data.Map.Strict            as Map
                                                ( insert
                                                , lookup
                                                )
import "extensible" Data.Extensible             ( Eff
                                                , peelEff0
                                                )

import           DomainObjects.Talent           ( Talent(..) )
import qualified DomainObjects.Talent.ID       as Talent
                                                ( ID )
import qualified Effects.IO                    as IO
                                                ( HasEff
                                                , lift
                                                )
import           Effects.TalentRepository       ( TalentRepository(..) )
import qualified Effects.TalentRepository      as TalentRepository
                                                ( AnonEff
                                                , NamedEff
                                                )

run
    :: forall effs a
     . IO.HasEff effs
    => IORef (Map Talent.ID Talent)
    -> Eff (TalentRepository.NamedEff ': effs) a
    -> Eff effs a
run ref effs = peelEff0 pure interpret effs
  where
    interpret :: forall r . TalentRepository.AnonEff r -> (r -> Eff effs a) -> Eff effs a
    interpret (Find id) k = do
        repo <- IO.lift $ readIORef ref
        let mtalent = Map.lookup id repo
        k mtalent
    interpret (Store talent@Talent { id }) k = do
        IO.lift $ atomicModifyIORef' ref (\repo -> (Map.insert id talent repo, ()))
        k ()
