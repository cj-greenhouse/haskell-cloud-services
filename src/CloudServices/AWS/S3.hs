module CloudServices.AWS.S3 (
    S3Environment (..),
    getObjectInS3,
    putObjectInS3,
    listObjectsInS3
) where
--
import CloudServices.ObjectStore(ObjectContainer, ObjectKey, ObjectValue)

import Conduit
import Control.Exception                         (try, SomeException)
import Control.Lens                              (view)
import Control.Monad                             (void)
import Control.Monad.Trans.AWS                   (sinkBody, runAWST, Rs)
import qualified Control.Monad.Trans.AWS as AWS  (send, paginate)
import qualified Data.ByteString as BS           (concat)
import Network.AWS                               (runAWS, runResourceT)
import Network.AWS.Data.Body                     (RqBody(Hashed), toHashed)
import Network.AWS.Env                           (Env)
import Network.AWS.Pager                         (AWSPager)
import qualified Network.AWS.S3.GetObject as AWS (getObject, gorsBody)
import qualified Network.AWS.S3.PutObject as AWS (putObject)
import Network.AWS.S3.ListObjectsV2              (listObjectsV2, lovrsContents)
import qualified Network.AWS.S3.Types as AWS     (ObjectKey(..), BucketName(..), oKey, keyName)
--
class S3Environment m where
    s3Environment :: m Env

getObjectInS3 :: (S3Environment IO) => ObjectContainer -> ObjectKey -> IO ObjectValue
getObjectInS3 bucketName objectKey = do
    env <- s3Environment
    l <- runResourceT . runAWST env $ do
        let bn = AWS.BucketName bucketName
            ok = AWS.ObjectKey objectKey
        rs <- view AWS.gorsBody <$> AWS.send (AWS.getObject bn ok)
        sinkBody rs sinkList
    pure $ BS.concat l

putObjectInS3 :: (S3Environment IO) => ObjectContainer -> ObjectKey -> ObjectValue -> IO ()
putObjectInS3 bucketName objectKey content = do
    let req = AWS.putObject
                (AWS.BucketName bucketName)
                (AWS.ObjectKey objectKey)
                (Hashed $ toHashed content)
    env <- s3Environment
    maybeSucceeded <- (try $ void $ runResourceT . runAWS env $ AWS.send req) :: IO (Either SomeException ())
    case maybeSucceeded of
        Left err -> print err
        Right _  -> pure ()

listObjectsInS3 :: (S3Environment IO) => ObjectContainer -> IO [ObjectKey]
listObjectsInS3 bucketName = do
    responseStream <- paginate $ listObjectsV2 $ AWS.BucketName bucketName
    let ks = view lovrsContents =<< responseStream
    pure $  view  (AWS.keyName ' ') <$> (view AWS.oKey <$> ks)
    where
        paginate :: (AWSPager a) => a -> IO [Rs a]
        paginate request = do
            env <- s3Environment
            runResourceT $ runAWST env $ runConduit $ AWS.paginate request .| sinkList
