module CloudServices.ObjectStore (
    ObjectStore (..),
    ObjectContainer,
    ObjectKey,
    ObjectValue,
) where
--
import qualified Data.ByteString as BS (ByteString)
import           Data.Text       (Text)
--
type ObjectContainer = Text
type ObjectKey       = Text
type ObjectValue     = BS.ByteString

class ObjectStore m where
    getObject       :: ObjectContainer -> ObjectKey -> m ObjectValue
    putObject       :: ObjectContainer -> ObjectKey -> ObjectValue -> m ()
    listObjects     :: ObjectContainer -> m [ObjectKey]
