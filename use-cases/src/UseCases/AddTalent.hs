{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

module UseCases.AddTalent
    ( addTalent
    )
where


import "base"    Prelude                 hiding ( id )
import "extensible" Data.Extensible             ( Eff )

import           DomainObjects.Talent           ( Talent(..) )
import qualified DomainObjects.Talent.Name     as Talent
                                                ( Name )
import qualified DomainObjects.Talent.AddedAt  as TalentAddedAt
                                                ( fromUTCTime )
import qualified Effects.Clock                 as Clock
                                                ( ask
                                                , HasEff
                                                )
import qualified Effects.TalentIDGenerator     as TalentIDGenerator
                                                ( HasEff
                                                , next
                                                )
import qualified Effects.TalentRepository      as TalentRepository
                                                ( HasEff
                                                , store
                                                )

addTalent
    :: forall effs
     . (Clock.HasEff effs, TalentIDGenerator.HasEff effs, TalentRepository.HasEff effs)
    => Talent.Name
    -> Eff effs Talent
addTalent name = do
    now <- Clock.ask
    id  <- TalentIDGenerator.next

    let addedAt = TalentAddedAt.fromUTCTime now
        talent  = Talent { id, name, addedAt }

    TalentRepository.store talent

    pure talent
