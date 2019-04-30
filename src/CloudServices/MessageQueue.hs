module CloudServices.MessageQueue where
--
import Data.Text (Text)
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
