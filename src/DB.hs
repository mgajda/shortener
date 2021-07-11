-- | This module simulates a database.
--
--   If we want it last beyond a transient, we should use `beam` to store the mapping instead.
module DB(DB, withDB, newURL, lookupURL) where

import qualified Data.IntMap   as Map
import           Data.Text(Text)
import           System.Random ( Random(randomR), getStdRandom )
import           Control.Concurrent.MVar as MVar

import           URLId

-- | Database connection simulated with shared in-memory dictionary.
newtype DB = DB (MVar.MVar (Map.IntMap Text))

-- | Perform an action with a database.
withDB    :: (DB -> IO a) -> IO a
withDB act = do
  db  <- DB <$> newMVar Map.empty
  act db

-- | Add new URL to database with random identifier.
--
--   May fail and yield nothing, if we clash.
newURL               :: DB -> Text -> IO (Maybe URLId)
newURL (DB ref) anURL =
  modifyMVar ref $ \aMap -> do
    index <- getStdRandom $ randomR (0, URLId.maxRange - 1)
    let (result, newMap) = Map.insertLookupWithKey (\_key _ old -> old) (fromIntegral index) anURL aMap
    case result of
      Nothing -> -- this is a new key
                 return (newMap, mkURLId index)
      Just _  -> return (aMap,   Nothing      ) -- old key, user needs to retry

-- | Lookup URL in a database.
lookupURL :: DB -> URLId -> IO (Maybe Text)
lookupURL (DB mapRef) urlid = do
  aMap <- readMVar mapRef
  return $! Map.lookup (fromIntegral (urlIdToInteger urlid)) aMap
