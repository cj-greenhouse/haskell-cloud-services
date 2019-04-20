module MailExchange (
    MailExchange(..),
    MailExchangeEmail,
    MailExchangeEmailAddress,
    sendEmailInSes
) where
--
import Control.Monad (void)
import Control.Monad.Trans.AWS (runAWST, Region(..))
import Control.Lens ((.~), set)
import Data.Function ((&))
import Data.Text (Text)
import Network.AWS (runResourceT, send, Credentials(Discover) )
import Network.AWS.Env (Env, newEnv, envRegion)
import Network.AWS.SES (SendEmail, sendEmail)
import Network.AWS.SES.Types (body, bText, content, destination, dToAddresses, message)
--
class MailExchange m where
    sendMail :: MailExchangeEmail -> m ()

newtype MailExchangeEmailAddress = EmailAddress { emailAddress :: Text }
    deriving (Eq, Show)

data MailExchangeEmail = Email {
    toAddresses :: [MailExchangeEmailAddress],
    fromAddress :: MailExchangeEmailAddress,
    emailSubject :: Text,
    emailBody :: Text
    } deriving (Eq, Show)

sendEmailInSes :: MailExchangeEmail -> IO ()
sendEmailInSes e = do
    env <- defaultEnvironment
    void $ runResourceT $ runAWST env $ send $ buildSESEmail
        (emailAddress $ fromAddress e)
        (map emailAddress $ toAddresses e)
        (emailSubject e)
        (emailBody e)

buildSESEmail :: Text -> [Text] -> Text -> Text -> SendEmail
buildSESEmail fromAddress toAddresses subjectText bodyText =
    let
        destinationContent = destination & dToAddresses .~ toAddresses
        bodyContent = body & bText .~ Just (content bodyText)
        subjectContent = content subjectText
        messageContent = message subjectContent bodyContent
    in sendEmail fromAddress destinationContent messageContent

-- TODO Move to type class or as a wiring partial applied to sendEmailSes ???
defaultEnvironment :: IO Env
defaultEnvironment = set envRegion NorthVirginia <$> newEnv Discover
