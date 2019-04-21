module KeyValueStore where
--
import Data.Text        (Text)
import Network.AWS.Env  (Env)
--
type KeyValueContainer              = Text
type Key a                          = a
type Value b                        = b
type KeyValuePutResponseStatus      = Int
type KeyValueChangeResponseStatus   = Int
type KeyValueDeleteResponseStatus   = Int

class KeyValueStore m a b where
    get     :: KeyValueContainer -> Key a -> m b
    put     :: KeyValueContainer -> Key a -> Value b -> m KeyValuePutResponseStatus
    change  :: KeyValueContainer -> Key a -> Value b -> m KeyValueChangeResponseStatus
    -- delete  :: forall b. KeyValueContainer -> Key a -> m KeyValueDeleteResponseStatus

class DynamoDBEnvironment m where
    dynamoDBEnvironment :: m Env