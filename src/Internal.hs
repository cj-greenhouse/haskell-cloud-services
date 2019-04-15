module Internal where
--
import Data.Text (Text)
--

newtype MailExchangeEmailAddress = EmailAddress { emailAddress :: Text }
    deriving (Eq, Show)

data MailExchangeEmail = Email {
    toAddresses :: [MailExchangeEmailAddress],
    fromAddress :: MailExchangeEmailAddress,
    emailSubject :: Text,
    emailBody :: Text
    } deriving (Eq, Show)