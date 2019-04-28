module CloudServices.AWS.DynamoDB where
--
import Network.AWS.Env  (Env)

class DynamoDBEnvironment m where
    dbdEnvironment :: m Env

-- getValueInDynamoDB :: (Monad IO, DynamoDBEnvironment IO) => KeyValueContainer -> k -> IO v
-- getValueInDynamoDB tableName key = do
--     env <- dbdEnvironment
--     response <- runResourceT $ runAWST env $ send $ getItem tableName
--     undefined

-- getItem tableName key =