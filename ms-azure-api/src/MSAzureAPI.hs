module MSAzureAPI (
    -- ** HTTP request helpers
  tryReq
  -- ** Common types
  , Collection
  , collectionValue
  , collectionNextLink
  -- *** Location
  , Location(..)
  , showLocation
                  ) where

import MSAzureAPI.Internal.Common
