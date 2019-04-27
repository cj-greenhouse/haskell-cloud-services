module MailExchange (
    MailExchange (..),
    MailAddress,
    MailToAddresses,
    MailFromAddress,
    MailSubject,
    MailBody,
    SendMailResponseStatus,
    SESEnvironment,
    sendMailInSES
) where
--
import Control.Monad.Trans.AWS  (runAWST)
import Control.Lens             ((^.), (.~))
import Data.Function            ((&))
import Data.Text                (Text)
import Network.AWS              (runResourceT, send)
import Network.AWS.Env          (Env)
import Network.AWS.SES          (SendEmail, sendEmail, sersResponseStatus)
import Network.AWS.SES.Types    (body, bText, content, destination, dToAddresses, message)
--
type MailToAddresses        = [MailAddress]
type MailFromAddress        = MailAddress
type MailSubject            = Text
type MailBody               = Text
type MailAddress            = Text
type SendMailResponseStatus = Int

class MailExchange m where
    sendMail :: MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> m SendMailResponseStatus

--
class SESEnvironment m where
    sesEnvironment :: m Env

sendMailInSES :: (Monad IO, SESEnvironment IO) => MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> IO SendMailResponseStatus
sendMailInSES fromAddress toAddresses mailSubject mailBody = do
    env <- sesEnvironment
    response <- runResourceT $ runAWST env $ send $ sesSendEmail fromAddress toAddresses mailSubject mailBody
    pure $ response ^. sersResponseStatus

sesSendEmail :: MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> SendEmail
sesSendEmail fromAddress toAddresses subjectText bodyText =
    let
        destinationContent = destination & dToAddresses .~ toAddresses
        bodyContent = body & bText .~ Just (content bodyText)
        subjectContent = content subjectText
        messageContent = message subjectContent bodyContent
    in sendEmail fromAddress destinationContent messageContent
