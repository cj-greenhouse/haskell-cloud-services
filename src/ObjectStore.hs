module ObjectStore (
    ObjectStore(..),
    getObjectInS3,
    putObjectInS3,
    listObjectsInS3
) where

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
import qualified Network.AWS.S3.GetObject as AWS (getObject, gorsBody)
import qualified Network.AWS.S3.PutObject as AWS (putObject)
import Network.AWS.S3.ListObjectsV2 (listObjectsV2, lovrsContents)
import qualified Network.AWS.S3.Types as AWS (ObjectKey(..), BucketName(..), oKey, keyName)

type ObjectContainer = Text
type ObjectKey       = Text
type ObjectValue     = B.ByteString

class ObjectStore m where
    getObject   :: ObjectContainer -> ObjectKey -> m ObjectValue
    putObject   :: ObjectContainer -> ObjectKey -> ObjectValue -> m ()
    listObjects :: ObjectContainer -> m [ObjectKey]

-- | get content to bucket using default credentails
getObjectInS3 :: ObjectContainer -> ObjectKey -> IO ObjectValue
getObjectInS3 bucket key = do
    env <- defaultEnvironment
    getObjectInS3ForEnv env bucket key

-- | get content to bucket using provided credentials
getObjectInS3ForEnv :: (HasEnv env) => env -> ObjectContainer -> ObjectKey -> IO ObjectValue
getObjectInS3ForEnv env bucket key = do
    l <- runResourceT . runAWST env $ do
        rs <- view AWS.gorsBody <$> (AWS.send $ AWS.getObject (AWS.BucketName bucket) (AWS.ObjectKey key))
        sinkBody rs sinkList
    pure $ B.concat l

-- | put content to bucket using default credentails
putObjectInS3 :: ObjectContainer -> ObjectKey -> ObjectValue -> IO ()
putObjectInS3 bucket key content = do
    env <- defaultEnvironment
    putObjectInS3ForEnv env bucket key content

-- | put content to bucket using provided credentials
putObjectInS3ForEnv :: (HasEnv env) => env -> ObjectContainer -> ObjectKey -> ObjectValue -> IO ()
putObjectInS3ForEnv env bucket key content = do
    let req = AWS.putObject
                (AWS.BucketName bucket)
                (AWS.ObjectKey key)
                (Hashed $ toHashed content)
    maybeSucceeded <- (try $ void $ runResourceT . runAWS env $ AWS.send req) :: IO (Either SomeException ())
    case maybeSucceeded of
        Left(err) -> putStrLn(show err)
        Right(_) -> pure ()

listObjectsInS3 :: ObjectContainer -> IO [ObjectKey]
listObjectsInS3 bucket = listObjectsInS3AllPages bucket

listObjectsInS3AllPages :: (Monad m, RunInAWS m) => ObjectContainer -> m [ObjectKey]
listObjectsInS3AllPages bucket = do
    responseStream <- paginate $ listObjectsV2 $ AWS.BucketName bucket
    let
        ks = join $ view lovrsContents <$> responseStream
    pure $  view  (AWS.keyName ' ') <$> (view AWS.oKey <$> ks)


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









