module CloudServices.AWS.Environment (
    awsDefaultEnv,
    awsNorthVirginiaDefaultEnv,
    awsNorthernCaliforniaDefaultEnv
) where
--
-- https://hackage.haskell.org/package/amazonka-1.6.1/docs/Network-AWS-Auth.html
-- https://hackage.haskell.org/package/amazonka-1.6.1/docs/Network-AWS-Env.html
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
