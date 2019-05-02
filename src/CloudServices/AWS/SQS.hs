module CloudServices.AWS.SQS(
    SQSEnvironment (..),
    sendMessageInSQS,
    receiveMessagesInSQS,
    deleteMessageInSQS
) where
--
-- amazonka: https://hackage.haskell.org/package/amazonka-sqs
--
import CloudServices.MessageQueue (Message (..), MessageQueueContainer, MessageReceiptHandle, MessageBody, SendMessageResponseStatus)

import Control.Lens                                    ((^.))
import Control.Monad                                   (void)
import Control.Monad.Trans.AWS                         (runAWST)
import Network.AWS                                     (runResourceT, send)
import Network.AWS.Env                                 (Env)
import qualified Network.AWS.SQS.DeleteMessage  as SQS (deleteMessage)                      -- https://hackage.haskell.org/package/amazonka-sqs-1.6.1/docs/Network-AWS-SQS-DeleteMessage.html
import qualified Network.AWS.SQS.ReceiveMessage as SQS (receiveMessage, rmrsMessages)       -- https://hackage.haskell.org/package/amazonka-sqs-1.6.1/docs/Network-AWS-SQS-ReceiveMessage.html
import qualified Network.AWS.SQS.SendMessage    as SQS (sendMessage, smrsResponseStatus)    -- https://hackage.haskell.org/package/amazonka-sqs-1.6.1/docs/Network-AWS-SQS-SendMessage.html
import Network.AWS.SQS.Types                           (mReceiptHandle, mBody)              -- https://hackage.haskell.org/package/amazonka-sqs-1.6.1/docs/Network-AWS-SQS-Types.html
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
