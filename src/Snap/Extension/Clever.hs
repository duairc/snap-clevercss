{-|

'Snap.Extension.Clever' exports the 'MonadClever' interface which allows you
to integrate CleverCSS templates into your Snap application. The interface's
operations are 'cleverServe', 'cleverServeSingle', and 'cleverRender'.

'Snap.Extension.Clever.Impl' contains the only implementation of this
interface and can be used to turn your application's monad into a
'MonadClever'.

-}

module Snap.Extension.Clever 
  ( MonadClever(..)
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Types

------------------------------------------------------------------------------
-- | The 'MonadClever' type class. Minimal complete definition:
-- 'cleverRender'.
class MonadSnap m => MonadClever m where
    -- | Renders a cleverCSS template as text\/css. If the given template is
    -- not found, this returns 'empty'.
    cleverRender :: ByteString -> m ()

    -- | Analogous to 'fileServe'. If the template specified in the request
    -- path is not found, it returns 'empty'.
    cleverServe :: m ()
    cleverServe = fmap rqPathInfo getRequest >>= cleverRender

    -- | Analogous to 'fileServeSingle'. If the given template is not found,
    -- this throws an error.
    cleverServeSingle :: ByteString -> m ()
    cleverServeSingle t = cleverRender t
        <|> error ("CleverCSS template " ++ show t ++ " not found.")
