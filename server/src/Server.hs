{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Server
    ( run
    )
where

import "aeson"   Data.Aeson                     ( (.=)
                                                , object
                                                )
import "base"    Control.Monad.IO.Class         ( MonadIO(liftIO) )
import "base"    Data.IORef                     ( newIORef
                                                , readIORef
                                                )
import "extensible" Data.Extensible             ( Eff
                                                , retractEff
                                                )
import "http-types" Network.HTTP.Types          ( status201 )
import "scotty"  Web.Scotty                     ( get
                                                , json
                                                , param
                                                , post
                                                , scotty
                                                , status
                                                )

import qualified DomainObjects.Talent.Name     as TalentName
                                                ( shamefullyConvertFromText )
import qualified Effects.Clock                 as Clock
                                                ( NamedEff )
import qualified Effects.Clock.Handlers.IO     as Clock
                                                ( run )
import qualified Effects.IO                    as IO
                                                ( NamedEff )
import qualified Effects.TalentIDGenerator     as TalentIDGenerator
                                                ( NamedEff )
import qualified Effects.TalentIDGenerator.Handlers.UUIDV4
                                               as TalentIDGenerator
                                                ( run )
import qualified Effects.TalentRepository      as TalentRepository
                                                ( NamedEff )
import qualified Effects.TalentRepository.Handlers.IORef
                                               as TalentRepository
                                                ( run )
import           UseCases.AddTalent             ( addTalent )

run :: IO ()
run = do
    talentRepo <- newIORef mempty

    let runUseCase
            :: forall a
             . Eff '[TalentRepository.NamedEff, TalentIDGenerator.NamedEff, Clock.NamedEff, IO.NamedEff] a
            -> IO a
        runUseCase = retractEff . Clock.run . TalentIDGenerator.run . TalentRepository.run talentRepo

    scotty 3000 $ do
        get "/talents.count" $ do
            repo <- liftIO $ readIORef talentRepo
            let count = length repo
            json $ object ["count" .= count]

        post "/talents.add" $ do
            name <- param "name"
            _    <- liftIO . runUseCase $ addTalent (TalentName.shamefullyConvertFromText name)
            status status201
            json $ object ["ok" .= True]
