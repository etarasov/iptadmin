
module IptAdmin.Types where

import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Map
import Data.Time
import Happstack.Server.SimpleHTTP

-- | Authorization monad
type IptAdminAuth = ServerPartT (ErrorT String IO)

-- | Main request handler monad
type IptAdmin = ServerPartT (ErrorT String (StateT MainState IO))

type SessionId = String

type Sessions = Map SessionId Session

data Session = Session { lastVisit :: UTCTime
                       , backup :: Maybe String
                       }

type MainState = (SessionId, IORef Sessions, IptAdminConfig)

data IptAdminConfig = IptAdminConfig { cSaveCommand :: String
                                     , cPort :: Int
                                     , cPamName :: String
                                     }
