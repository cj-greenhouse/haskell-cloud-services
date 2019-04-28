module CloudServices.AWS.SQS(
    SQSEnvironment (..),
    sendMessageInSQS,
    receiveMessagesInSQS,
    deleteMessageInSQS
) where
--
import CloudServices.MessageQueue (Message (..), MessageQueueContainer, MessageReceiptHandle, MessageBody, SendMessageResponseStatus)

import Control.Lens                                    ((^.))
import Control.Monad                                   (void)
import Control.Monad.Trans.AWS                         (runAWST)
import Network.AWS                                     (runResourceT, send)
import Network.AWS.Env                                 (Env)
import qualified Network.AWS.SQS.DeleteMessage  as SQS (deleteMessage)
import qualified Network.AWS.SQS.ReceiveMessage as SQS (receiveMessage, rmrsMessages)
import qualified Network.AWS.SQS.SendMessage    as SQS (sendMessage, smrsResponseStatus)
import Network.AWS.SQS.Types                           (mReceiptHandle, mBody)
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
