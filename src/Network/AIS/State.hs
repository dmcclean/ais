module Network.AIS.State where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Word (Word16)
import Network.AIS (AisMessage(..))
import Network.AIS.Vocabulary

type Slot = Word16
type SlotOffset = Word16

frameLength :: SlotOffset
frameLength = 2500

maxAge :: SlotOffset
maxAge = 8 * frameLength

data StationState = StationState { stationID :: MMSI
                                 , displayName :: Maybe Text
                                 , lastReceived :: Maybe Slot -- Nothing means a long time ago
                                 , lastReceivedChannel :: Channel
                                 , lastPosition :: Maybe (Longitude, Latitude)
                                 , mostRecentlyVictimized :: Maybe Slot
                                 , lastSyncState :: SyncState
                                 , lastNavigationState :: Maybe NavigationalStatus
                                 }
  deriving (Eq, Show)

data StationDirectory = StationDirectory { directoryLastUpdated :: Slot
                                         , ownStationID :: Maybe MMSI
                                         , stations :: Map MMSI StationState
                                         }
  deriving (Eq, Show)

-- parameters are own station ID, current slot
makeStationDirectory :: Maybe MMSI -> Slot -> StationDirectory
makeStationDirectory ownStation t = StationDirectory { directoryLastUpdated = t
                                                     , ownStationID = ownStation
                                                     , stations = M.empty
                                                     }

tickStationDirectory :: Slot -> StationDirectory -> StationDirectory
tickStationDirectory t d = d { directoryLastUpdated = t, stations = (ageOutVictimization . ageOut) <$> stations d }
  where
    -- because the slot numbers roll over quickly, we need to age out any entries that are past a certain point
    ageOut :: StationState -> StationState
    ageOut s | Just t' <- lastReceived s, t - t' > maxAge = s { lastReceived = Nothing }
             | otherwise = s
    ageOutVictimization :: StationState -> StationState
    ageOutVictimization s | Just t' <- mostRecentlyVictimized s, t - t' > maxAge = s { mostRecentlyVictimized = Nothing }
                          | otherwise = s

updateStationDirectory :: Slot -> Channel -> AisMessage -> StationDirectory -> StationDirectory
updateStationDirectory _ _ m d | Just (userID m) == ownStationID d = d -- ignore messages from own station
                               | otherwise = d
