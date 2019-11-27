module TestImport
    ( module X
    )
where

import           Test.Hspec                    as X
                                                ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
