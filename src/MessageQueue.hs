module MessageQueue (
    MessageQueue (..),
    SQSEnvironment (..),
    sendMessageInSQS,
    receiveMessageInSQS,
    deleteMessageInSQS
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

class MessageQueue m where
    sendMessage    :: MessageQueueContainer -> MessageBody -> m SendMessageResponseStatus
    receiveMessage :: MessageQueueContainer -> m [(Maybe MessageReceiptHandle, Maybe MessageBody)]
    deleteMessage  :: MessageQueueContainer -> MessageReceiptHandle -> m ()

class SQSEnvironment m where
    sqsEnvironment :: m Env

sendMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> MessageBody -> IO SendMessageResponseStatus
sendMessageInSQS queueUrl messageBody = do
    env <- sqsEnvironment
    response <- runResourceT $ runAWST env $ send $ SQS.sendMessage queueUrl messageBody
    pure $ response ^. SQS.smrsResponseStatus

receiveMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> IO [Maybe (MessageReceiptHandle, MessageBody)]
receiveMessageInSQS queueUrl = do
    env <- sqsEnvironment
    response <- runResourceT $ runAWST env $ send $ SQS.receiveMessage queueUrl
    pure $ map toResult $ response ^. SQS.rmrsMessages
    where
        toResult r = case (r ^. mReceiptHandle, r ^. mBody) of
            (Just h, Just b) -> Just (h, b)
            _                -> Nothing

deleteMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> MessageReceiptHandle -> IO ()
deleteMessageInSQS queueUrl messageHandle = do
    env <- sqsEnvironment
    void $ runResourceT $ runAWST env $ send $ SQS.deleteMessage queueUrl messageHandle