module KeyValueStore where
--
import Control.Monad.Trans.AWS                         (runAWST)
import Data.Text                                       (Text)
import Network.AWS                                     (runResourceT, send)
import Network.AWS.Env  (Env)
--
type KeyValueContainer              = Text
type KeyValuePutResponseStatus      = Int
type KeyValueChangeResponseStatus   = Int
type KeyValueDeleteResponseStatus   = Int

class KeyValueStore m k v where
    getValue     :: KeyValueContainer -> k -> m v
    putValue     :: KeyValueContainer -> k -> v -> m KeyValuePutResponseStatus
    changeValue  :: KeyValueContainer -> k -> v -> m KeyValueChangeResponseStatus
    -- deleteValue  :: forall v. KeyValueContainer -> k -> m KeyValueDeleteResponseStatus

class DynamoDBEnvironment m where
    dbdEnvironment :: m Env

-- getValueInDynamoDB :: (Monad IO, DynamoDBEnvironment IO) => KeyValueContainer -> k -> IO v
-- getValueInDynamoDB tableName key = do
--     env <- dbdEnvironment
--     response <- runResourceT $ runAWST env $ send $ getItem tableName
--     undefined

-- getItem tableName key =