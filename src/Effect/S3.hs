module Effect.S3 where

import Conduit
import Control.Exception(try, SomeException)
import Control.Lens (view)
import Control.Monad (void, join)
import Control.Monad.Trans.AWS (sinkBody, runAWST, Rs)
import qualified Control.Monad.Trans.AWS as AWS (send, paginate)
import qualified Data.ByteString as B (ByteString, concat)
import Data.Text (Text)
import Network.AWS (HasEnv, newEnv, Credentials(..), runAWS, runResourceT)
import Network.AWS.Data.Body (RqBody(Hashed), toHashed)
import Network.AWS.Env (Env)
import Network.AWS.Pager (AWSPager)
import Network.AWS.S3.GetObject (getObject, gorsBody)
import Network.AWS.S3.PutObject (putObject)
import Network.AWS.S3.ListObjectsV2 (listObjectsV2, lovrsContents)
import Network.AWS.S3.Types (ObjectKey(..), BucketName(..), oKey, keyName)

type S3BucketName = Text
type S3ObjectKey = Text
type S3ObjectValue = B.ByteString

-- | get content to bucket using default credentails
getS3Object :: S3BucketName -> S3ObjectKey -> IO S3ObjectValue
getS3Object bucket key = do
    env <- defaultEnvironment
    getS3ObjectForEnv env bucket key

-- | get content to bucket using provided credentials
getS3ObjectForEnv :: (HasEnv env) => env -> S3BucketName -> S3ObjectKey -> IO S3ObjectValue
getS3ObjectForEnv env bucket key = do
    l <- runResourceT . runAWST env $ do
        rs <- view gorsBody <$> (AWS.send $ getObject (BucketName bucket) (ObjectKey key))
        sinkBody rs sinkList
    pure $ B.concat l

-- | put content to bucket using default credentails
putS3Object :: S3BucketName -> S3ObjectKey -> S3ObjectValue -> IO ()
putS3Object bucket key content = do
    env <- defaultEnvironment
    putS3ObjectForEnv env bucket key content

-- | put content to bucket using provided credentials
putS3ObjectForEnv :: (HasEnv env) => env -> S3BucketName -> S3ObjectKey -> S3ObjectValue -> IO ()
putS3ObjectForEnv env bucket key content = do
    let req = putObject
                (BucketName bucket)
                (ObjectKey key)
                (Hashed $ toHashed content)
    maybeSucceeded <- (try $ void $ runResourceT . runAWS env $ AWS.send req) :: IO (Either SomeException ())
    case maybeSucceeded of
        Left(err) -> putStrLn(show err)
        Right(_) -> pure ()

listS3Objects :: S3BucketName -> IO [S3ObjectKey]
listS3Objects bucket = listS3ObjectsAllPages bucket

listS3ObjectsAllPages :: (Monad m, RunInAWS m) => S3BucketName -> m [S3ObjectKey]
listS3ObjectsAllPages bucket = do
    responseStream <- paginate $ listObjectsV2 $ BucketName bucket
    let
        ks = join $ view lovrsContents <$> responseStream
    pure $  view  (keyName ' ') <$> (view oKey <$> ks)



-- TODO make this part of RUNINAWS ????
defaultEnvironment :: IO Env
defaultEnvironment = newEnv Discover

class RunInAWS m where
    paginate :: (AWSPager a) => a -> m [Rs a]

instance RunInAWS IO where
    paginate :: (AWSPager a) => a -> IO [Rs a]
    paginate request = do
        env <- defaultEnvironment
        runResourceT $ runAWST env $ runConduit $ AWS.paginate request .| sinkList









