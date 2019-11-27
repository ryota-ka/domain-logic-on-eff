{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module UseCases.AddTalentSpec
    ( spec
    )
where

import           TestImport

import "containers" Data.Map                    ( Map )
import "extensible" Data.Extensible             ( Eff
                                                , leaveEff
                                                )
import "time"    Data.Time.Clock                ( UTCTime )
import "time"    Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )

import           DomainObjects.Talent           ( Talent )
import qualified DomainObjects.Talent.ID       as Talent
                                                ( ID )
import qualified DomainObjects.Talent.Name     as TalentName
                                                ( shamefullyConvertFromText )
import qualified Effects.Clock                 as Clock
                                                ( NamedEff )
import qualified Effects.Clock.Handlers.Pure   as Clock
                                                ( run )
import qualified Effects.TalentIDGenerator     as TalentIDGenerator
                                                ( NamedEff )
import qualified Effects.TalentIDGenerator.Handlers.Pure
                                               as TalentIDGenerator
                                                ( run )
import qualified Effects.TalentRepository      as TalentRepository
                                                ( NamedEff )
import qualified Effects.TalentRepository.Handlers.Pure
                                               as TalentRepository
                                                ( run )
import           UseCases.AddTalent             ( addTalent )

now :: UTCTime
now = posixSecondsToUTCTime 1234567890

runUseCase
    :: Eff '[TalentRepository.NamedEff, TalentIDGenerator.NamedEff, Clock.NamedEff] a -> (a, Map Talent.ID Talent)
runUseCase = leaveEff . Clock.run now . TalentIDGenerator.run . TalentRepository.run mempty

spec :: Spec
spec = describe "addTalent" $ do
    it "increments the number of talents in the repository" $ do
        let name      = TalentName.shamefullyConvertFromText "John Doe"
            (_, repo) = runUseCase $ addTalent name
        length repo `shouldBe` 1
