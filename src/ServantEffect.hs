module ServantEffect 
  (
    ServantClient(..),
    runClient,
    runServantClient
  ) where 

import Control.Monad ( (>=>) )
import Data.Text (Text)
import qualified Data.Text as T

import Servant.Client
import Network.HTTP.Client.TLS (newTlsManager)

import Polysemy 
import Polysemy.Error

-- AUTHENTICATION 
-- POLYSEMY EFFECT 
-- copied with changes from https://github.com/AJChapman/servant-polysemy/blob/master/src/Servant/Polysemy/Client.hs 

data ServantClient m a where
  RunClient'  :: Text -> ClientM o -> ServantClient m (Either ClientError o)

makeSem ''ServantClient

runClient
  :: Members '[ServantClient, Error ClientError] r
  => Text 
  -> ClientM o 
  -> Sem r o
runClient url = runClient' url >=> fromEither

runServantClient
  :: Members '[Embed IO] r
  => Sem (ServantClient ': r) a -> Sem r a
runServantClient m = do
  manager' <- embed newTlsManager
  interpret (\case
    RunClient' url client' -> do
      url' <- embed $ parseBaseUrl $ T.unpack url
      let env = mkClientEnv manager' url'
      embed $ runClientM client' env
    ) m