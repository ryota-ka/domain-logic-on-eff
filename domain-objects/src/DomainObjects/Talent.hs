module DomainObjects.Talent
    ( Talent(..)
    )
where

import           DomainObjects.Talent.AddedAt   ( AddedAt )
import           DomainObjects.Talent.ID        ( ID )
import           DomainObjects.Talent.Name      ( Name )

data Talent
    = Talent
    { id :: ID
    , name :: Name
    , addedAt :: AddedAt
    }
