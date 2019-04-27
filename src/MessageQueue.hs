module MessageQueue (
    MessageQueue (..),
    SQSEnvironment (..),
    sendMessageInSQS,
    receiveMessagesInSQS,
    deleteMessageInSQS,
    first
) where
--
import Control.Lens                                    ((^.))
import Control.Monad                                   (void)
import Control.Monad.Trans.AWS                         (runAWST)
import Data.Text                                       (Text)
import Network.AWS                                     (runResourceT, send)
import Network.AWS.Env                                 (Env)
import qualified Network.AWS.SQS.DeleteMessage  as SQS (deleteMessage)
import qualified Network.AWS.SQS.ReceiveMessage as SQS (receiveMessage, rmrsMessages)
import qualified Network.AWS.SQS.SendMessage    as SQS (sendMessage, smrsResponseStatus)
import Network.AWS.SQS.Types                           (mReceiptHandle, mBody)
--
type MessageBody                = Text      --SQS limits to 256KB
type MessageReceiptHandle       = Text
type MessageQueueContainer      = Text
type SendMessageResponseStatus  = Int

data Message =
    Message MessageReceiptHandle MessageBody
    deriving (Eq, Show)

class MessageQueue m where
    sendMessage     :: MessageQueueContainer -> MessageBody -> m SendMessageResponseStatus
    receiveMessages :: MessageQueueContainer -> m [Maybe Message]
    deleteMessage   :: MessageQueueContainer -> MessageReceiptHandle -> m ()

first :: [Maybe Message] -> Maybe Message
first [] = Nothing
first xs = head xs
--
class SQSEnvironment m where
    sqsEnvironment :: m Env

sendMessageInSQS :: (SQSEnvironment IO) => MessageQueueContainer -> MessageBody -> IO SendMessageResponseStatus
sendMessageInSQS queueUrl messageBody = do
    env <- sqsEnvironment
    response <- runResourceT $ runAWST env $ send $ SQS.sendMessage queueUrl messageBody
    pure $ response ^. SQS.smrsResponseStatus

receiveMessagesInSQS :: (SQSEnvironment IO) => MessageQueueContainer -> IO [Maybe Message]
receiveMessagesInSQS queueUrl = do
    env <- sqsEnvironment
    response <- runResourceT $ runAWST env $ send $ SQS.receiveMessage queueUrl
    pure $ map toResult $ response ^. SQS.rmrsMessages
    where
        toResult r = case (r ^. mReceiptHandle, r ^. mBody) of
            (Just h, Just b) -> Just $ Message h b
            _                -> Nothing

deleteMessageInSQS :: (SQSEnvironment IO) => MessageQueueContainer -> MessageReceiptHandle -> IO ()
deleteMessageInSQS queueUrl messageHandle = do
    env <- sqsEnvironment
    void $ runResourceT $ runAWST env $ send $ SQS.deleteMessage queueUrl messageHandle