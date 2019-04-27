module AWS.Environment (
    defaultEnvironment,
    northVirginiaDefaultEnvironment,
    northernCaliforniaDefaultEnvironment
) where
--
import Control.Lens             (set)
import Control.Monad.Trans.AWS  (Region(NorthCalifornia, NorthVirginia))
import Network.AWS.Auth         (Credentials(Discover))
import Network.AWS.Env          (Env, newEnv, envRegion)
--
defaultEnvironment :: IO Env
defaultEnvironment = newEnv Discover

northVirginiaDefaultEnvironment :: IO Env
northVirginiaDefaultEnvironment = defaultEnvironment `withRegion` NorthVirginia

northernCaliforniaDefaultEnvironment :: IO Env
northernCaliforniaDefaultEnvironment = defaultEnvironment `withRegion` NorthCalifornia

withRegion :: IO Env -> Region -> IO Env
withRegion env region = set envRegion region <$> env
