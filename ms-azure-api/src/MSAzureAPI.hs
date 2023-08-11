module MSAzureAPI (
  -- * HTTP request helpers
  tryReq
  -- * Common types
  -- ** Collection
  , Collection
  , collectionValue
  , collectionNextLink
  -- ** Location
  , Location(..)
  , showLocation
  , locationDisplayName
  ) where

import MSAzureAPI.Internal.Common
