module Internal where
--
import Data.Text (Text)
--

newtype EmailAddress = EmailAddress { email :: Text }
    deriving (Eq, Show)

data Email = Email {
    toAddresses :: [EmailAddress],
    fromAddress :: EmailAddress,
    emailSubject :: Text,
    emailBody :: Text
    } deriving (Eq, Show)