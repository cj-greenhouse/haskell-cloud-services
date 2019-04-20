module MessageQueue (
    MessageQueue (..),
    SQSEnvironment (..),
    sendMessageInSQS,
    receiveMessageInSQS,
    deleteMessageInSQS
) where
--
import Control.Monad (void)
import Control.Monad.Trans.AWS (runAWST)
import Data.Text (Text)
import Network.AWS (runResourceT, send)
import Network.AWS.Env (Env)
import qualified Network.AWS.SQS.DeleteMessage as SQS (deleteMessage)
import qualified Network.AWS.SQS.ReceiveMessage as SQS (receiveMessage)
import qualified Network.AWS.SQS.SendMessage as SQS (sendMessage)
--
type MessageBody            = Text      --SQS limits to 256KB
type MessageHandle          = Text
type MessageQueueContainer  = Text

class MessageQueue m where
    sendMessage     :: MessageQueueContainer -> MessageBody -> m ()
    receiveMessage  :: MessageQueueContainer -> m (MessageHandle, MessageBody)
    deleteMessage   :: MessageQueueContainer -> MessageHandle -> m ()

class SQSEnvironment m where
    sqsEnvironment :: m Env

--TODO return the response for inspection if needed??
sendMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> MessageBody -> IO ()
sendMessageInSQS queueUrl messageBody = do
    env <- sqsEnvironment
    void $ runResourceT $ runAWST env $ send $ SQS.sendMessage queueUrl messageBody

receiveMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> IO (MessageHandle, MessageBody)
receiveMessageInSQS queueUrl = do
    env <- sqsEnvironment
    response <- runResourceT $ runAWST env $ send $ SQS.receiveMessage queueUrl
    undefined

--TODO return the response for inspection if needed??
deleteMessageInSQS :: (Monad IO, SQSEnvironment IO) => MessageQueueContainer -> MessageHandle -> IO ()
deleteMessageInSQS queueUrl messageHandle = do
    env <- sqsEnvironment
    void $ runResourceT $ runAWST env $ send $ SQS.deleteMessage queueUrl messageHandle