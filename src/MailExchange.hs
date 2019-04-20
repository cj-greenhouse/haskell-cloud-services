module MailExchange (
    MailExchange(..),
    MailAddress,
    SESEnvironment,
    sendMailInSES
) where
--
import Control.Monad (void)
import Control.Monad.Trans.AWS (runAWST, Region(NorthVirginia))
import Control.Lens ((.~), set)
import Data.Function ((&))
import Data.Text (Text)
import Network.AWS (runResourceT, send, Credentials(Discover) )
import Network.AWS.Env (Env, newEnv, envRegion)
import Network.AWS.SES (SendEmail, sendEmail)
import Network.AWS.SES.Types (body, bText, content, destination, dToAddresses, message)
--
type MailToAddresses = [MailAddress]
type MailFromAddress = MailAddress
type MailSubject     = Text
type MailBody        = Text
newtype MailAddress  = EmailAddress { emailAddress :: Text } deriving (Eq, Show)

class MailExchange m where
    sendMail :: MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> m ()

class SESEnvironment m where
    sesEnvironment :: m Env

sendMailInSES :: (Monad IO, SESEnvironment IO) => MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> IO ()
sendMailInSES fromAddress toAddresses mailSubject mailBody = do
    env <- sesEnvironment
    void $ runResourceT $ runAWST env $ send $ buildSESSendEmail
        (emailAddress fromAddress)
        (map emailAddress toAddresses)
        (mailSubject)
        (mailBody)

buildSESSendEmail :: Text -> [Text] -> Text -> Text -> SendEmail
buildSESSendEmail fromAddress toAddresses subjectText bodyText =
    let
        destinationContent = destination & dToAddresses .~ toAddresses
        bodyContent = body & bText .~ Just (content bodyText)
        subjectContent = content subjectText
        messageContent = message subjectContent bodyContent
    in sendEmail fromAddress destinationContent messageContent
