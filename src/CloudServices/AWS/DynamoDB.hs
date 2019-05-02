module CloudServices.AWS.DynamoDB (
    DynamoDBEnvironment
) where
--
-- https://hackage.haskell.org/package/amazonka-dynamodb
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