{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Clever.Impl' is an implementation of the 'MonadClever'
interface defined in 'Snap.Extension.Clever'.

As always, to use, add 'CleverState' to your application's state, along with
an instance of 'HasCleverState' for your application's state, making sure to
use 'cleverInitializer' in your application's 'Initializer', and then you're
ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.Clever.Impl
  ( 
    -- * Clever State Definitions
    CleverState
  , HasCleverState(..)
  , cleverInitializer

    -- * The MonadClever Interface
  , MonadClever(..)
  ) where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import           Data.Either
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Data.Maybe.HT
import           Data.String
import qualified Data.Traversable as T
import           Generics.Pointless.Combinators (subr)
import           Snap.Extension
import           Snap.Extension.Clever
import           Snap.Types hiding (dir, path)
import           System.Directory
import           System.Directory.Tree
import           Text.CSS.CleverCSS


------------------------------------------------------------------------------
-- | Your application's state must include a 'CleverState' in order for your
-- application to be a 'MonadClever'.
data CleverState = CleverState
    { _path     :: FilePath
    , _mapping  :: MVar (Map ByteString ByteString)
    }


------------------------------------------------------------------------------
-- | For you appliaction's monad to be a 'MonadClever', your application's
-- state needs to be an instance of 'HasCleverState'. Minimal complete
-- definition: 'getCleverState', 'setCleverState'.
class HasCleverState s where
    getCleverState :: s -> CleverState
    setCleverState :: CleverState -> s -> s

    modifyCleverState :: (CleverState -> CleverState) -> s -> s
    modifyCleverState f s = setCleverState (f $ getCleverState s) s


------------------------------------------------------------------------------
-- | The 'Initializer' for 'CleverState'. It takes one argument, a path to a
-- template directory containing @.clever@ files.
cleverInitializer :: FilePath -> Initializer CleverState
cleverInitializer path = do
    cleverState <- liftIO $ do
        templates <- loadTemplates path
        either error (fmap (CleverState path) . newMVar) templates
    mkInitializer cleverState


------------------------------------------------------------------------------
instance InitializerState CleverState where
    extensionId = const "Clever/Impl"
    mkCleanup   = const $ return ()
    mkReload (CleverState path mapping) = do
        templates <- loadTemplates path
        either error (modifyMVar_ mapping . const . return) templates


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'cleverRender'.
getTemplate :: (MonadSnap m, HasCleverState s, MonadReader s m)
            => ByteString -> m (Maybe ByteString)
getTemplate p = do
    (CleverState _ mapping) <- asks getCleverState
    liftIO $ readMVar mapping >>= return . M.lookup p


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'cleverRender'.
serve :: MonadSnap m => ByteString -> m ()
serve css = do
    modifyResponse $ setContentType "text/css; charset=utf-8"
    modifyResponse $ setContentLength (fromIntegral $ B.length css)
    writeBS css


------------------------------------------------------------------------------
instance HasCleverState s => MonadClever (SnapExtend s) where
    cleverRender path = getTemplate path >>= maybe pass serve


------------------------------------------------------------------------------
instance (MonadSnap m, HasCleverState s) => MonadClever (ReaderT s m) where
    cleverRender path = getTemplate path >>= maybe pass serve


------------------------------------------------------------------------------
-- | Given the path to a @.clever@ file, this returns either a 'ByteString'
-- of the @.clever@ file rendered as CSS, or an error message.
loadTemplate :: FilePath -> IO (Either String ByteString)
loadTemplate f = do
    cleverCSS <- readFile f
    css <- cleverCSSConvert f cleverCSS []
    return $ right U.fromString $ css


------------------------------------------------------------------------------
-- | Given the path to a directory containing @.clever@ files, this returns a
-- map from the names of those @.clever@ files with the @.clever@ replaced
-- with @.css@ to the contents of those files after being processed by the
-- 'cleverCSSConvert' function.
loadTemplates :: FilePath -> IO (Either String (Map ByteString ByteString))
loadTemplates path = readDirectoryWith reader path
    >>= (free
        -- DirTree (Maybe (FilePath, Either String ByteString))
    >>> F.toList
        -- [Maybe (String, Either String ByteString)]
    >>> catMaybes
        -- [(String, Either String ByteString)]
    >>> unzip
        -- ([String], [Either String ByteString])
    >>> second partitionEithers
        -- ([String], ([String], [ByteString]))
    >>> second (first unlines)
        -- ([String], (String, [ByteString]))
    >>> subr
        -- (String, ([String], [ByteString]))
    >>> second (uncurry zip)
        -- (String, [(String, ByteString)])
    >>> second (map . first $ U.fromString)
        -- (String, [(ByteString, ByteString)])
    >>> second M.fromList
        -- (String, Map ByteString ByteString)
    >>> (toEither =<< not . null . fst)
        -- Either String (Map ByteString ByteString)
    >>> return)
  where
    reader file = T.sequence $ toMaybe (".clever" `isSuffixOf` file) $ do
        template <- loadTemplate file
        return (toDotCss $ drop (length path + 1) file, template)
    toDotCss   = (++ ".css") . dropLast (length (".clever" :: [Char]))
    dropLast n = reverse . drop n . reverse
    toEither p (a, b) = if p then Left a else Right b
