module Lib where
--
import Data.Text (Text)
import Data.ByteString (ByteString)
import Internal
--

type Container = Text
type Key = Text
type Value = ByteString
class ObjectStore m where
    getObject   :: Container -> Key -> m Value
    putObject   :: Container -> Key -> Value -> m ()
    listObjects :: Container -> m [Key]

class MailExchange m where
    sendMail :: Email -> m ()