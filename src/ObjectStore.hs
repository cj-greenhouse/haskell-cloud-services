module ObjectStore (
    ObjectStore (..),
    ObjectContainer,
    ObjectKey,
    ObjectValue,
    S3Environment (..),
    getObjectInS3,
    putObjectInS3,
    listObjectsInS3
) where
--
import Conduit
import Control.Exception                            (try, SomeException)
import Control.Lens                                 (view)
import Control.Monad                                (void, join)
import Control.Monad.Trans.AWS                      (sinkBody, runAWST, Rs)
import qualified Control.Monad.Trans.AWS    as AWS  (send, paginate)
import qualified Data.ByteString            as BS   (ByteString, concat)
import Data.Text                                    (Text)
import Network.AWS                                  (runAWS, runResourceT)
import Network.AWS.Data.Body                        (RqBody(Hashed), toHashed)
import Network.AWS.Env                              (Env)
import Network.AWS.Pager                            (AWSPager)
import qualified Network.AWS.S3.GetObject   as AWS  (getObject, gorsBody)
import qualified Network.AWS.S3.PutObject   as AWS  (putObject)
import Network.AWS.S3.ListObjectsV2                 (listObjectsV2, lovrsContents)
import qualified Network.AWS.S3.Types       as AWS  (ObjectKey(..), BucketName(..), oKey, keyName)
--
type ObjectContainer = Text
type ObjectKey       = Text
type ObjectValue     = BS.ByteString

class ObjectStore m where
    getObject       :: ObjectContainer -> ObjectKey -> m ObjectValue
    putObject       :: ObjectContainer -> ObjectKey -> ObjectValue -> m ()
    listObjects     :: ObjectContainer -> m [ObjectKey]

class S3Environment m where
    s3Environment   :: m Env

--
-- | get content to bucket using provided S3 credentials
getObjectInS3 :: (S3Environment IO) => ObjectContainer -> ObjectKey -> IO ObjectValue
getObjectInS3 bucketName objectKey = do
    env <- s3Environment
    l <- runResourceT . runAWST env $ do
        rs <- view AWS.gorsBody <$> (AWS.send $ AWS.getObject (AWS.BucketName bucketName) (AWS.ObjectKey objectKey))
        sinkBody rs sinkList
    pure $ BS.concat l

-- | put content to bucket using provided credentials
putObjectInS3 :: (S3Environment IO) => ObjectContainer -> ObjectKey -> ObjectValue -> IO ()
putObjectInS3 bucketName objectKey content = do
    let req = AWS.putObject
                (AWS.BucketName bucketName)
                (AWS.ObjectKey objectKey)
                (Hashed $ toHashed content)
    env <- s3Environment
    maybeSucceeded <- (try $ void $ runResourceT . runAWS env $ AWS.send req) :: IO (Either SomeException ())
    case maybeSucceeded of
        Left(err) -> putStrLn(show err)
        Right(_) -> pure ()

listObjectsInS3 :: (S3Environment IO) => ObjectContainer -> IO [ObjectKey]
listObjectsInS3 bucketName = do
    responseStream <- paginate $ listObjectsV2 $ AWS.BucketName bucketName
    let ks = join $ view lovrsContents <$> responseStream
    pure $  view  (AWS.keyName ' ') <$> (view AWS.oKey <$> ks)
    where
        paginate :: (AWSPager a) => a -> IO [Rs a]
        paginate request = do
            env <- s3Environment
            runResourceT $ runAWST env $ runConduit $ AWS.paginate request .| sinkList









