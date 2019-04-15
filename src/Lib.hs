module Lib where
--
import Data.Text (Text)
import Data.ByteString (ByteString)
import Internal
--

type ObjectContainer = Text
type ObjectKey       = Text
type ObjectValue     = ByteString

class ObjectStore m where
    getObject   :: ObjectContainer -> ObjectKey -> m ObjectValue
    putObject   :: ObjectContainer -> ObjectKey -> ObjectValue -> m ()
    listObjects :: ObjectContainer -> m [ObjectKey]

class MailExchange m where
    sendMail :: MailExchangeEmail -> m ()