{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.TalentRepository.Handlers.Pure
    ( run
    )
where

import           Prelude                 hiding ( id )

import "containers" Data.Map.Strict             ( Map )
import "containers" Data.Map.Strict            as Map
                                                ( insert
                                                , lookup
                                                )
import "extensible" Data.Extensible             ( Eff
                                                , peelEff1
                                                )

import           DomainObjects.Talent           ( Talent(..) )
import qualified DomainObjects.Talent.ID       as Talent
                                                ( ID )
import           Effects.TalentRepository       ( TalentRepository(..) )
import qualified Effects.TalentRepository      as TalentRepository
                                                ( AnonEff
                                                , NamedEff
                                                )

run
    :: forall effs a
     . Map Talent.ID Talent
    -> Eff (TalentRepository.NamedEff ': effs) a
    -> Eff effs (a, Map Talent.ID Talent)
run initState effs = peelEff1 (\a s -> pure (a, s)) interpret effs initState
  where
    interpret
        :: forall r
         . TalentRepository.AnonEff r
        -> (r -> Map Talent.ID Talent -> Eff effs (a, Map Talent.ID Talent))
        -> Map Talent.ID Talent
        -> Eff effs (a, Map Talent.ID Talent)
    interpret (Find  id) k s = k (Map.lookup id s) s
    interpret (Store talent@Talent { id }) k s = let s' = Map.insert id talent s in k () s'
