module CloudServices.AWS.Environment (
    awsDefaultEnv,
    awsNorthVirginiaDefaultEnv,
    awsNorthernCaliforniaDefaultEnv
) where
--
import Control.Lens (set)
import Control.Monad.Trans.AWS (Region(NorthCalifornia, NorthVirginia))
import Network.AWS.Auth (Credentials(Discover))
import Network.AWS.Env (Env, newEnv, envRegion)
--
awsDefaultEnv :: IO Env
awsDefaultEnv = newEnv Discover

awsNorthVirginiaDefaultEnv :: IO Env
awsNorthVirginiaDefaultEnv = awsDefaultEnv `inRegion` NorthVirginia

awsNorthernCaliforniaDefaultEnv :: IO Env
awsNorthernCaliforniaDefaultEnv = awsDefaultEnv `inRegion` NorthCalifornia

inRegion :: IO Env -> Region -> IO Env
inRegion env region = set envRegion region <$> env
