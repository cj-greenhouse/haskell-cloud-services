module CloudServices.AWS.SES (
    SESEnvironment,
    sendMailInSES
) where
--
import CloudServices.MailExchange(MailFromAddress, MailToAddresses, MailSubject, MailBody, SendMailResponseStatus)

import Control.Monad.Trans.AWS  (runAWST)
import Control.Lens             ((^.), (?~), (.~))
import Data.Function            ((&))
import Network.AWS              (runResourceT, send)
import Network.AWS.Env          (Env)
import Network.AWS.SES          (SendEmail, sendEmail, sersResponseStatus)
import Network.AWS.SES.Types    (body, bText, content, destination, dToAddresses, message)
--
class SESEnvironment m where
    sesEnvironment :: m Env

sendMailInSES :: (SESEnvironment IO) => MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> IO SendMailResponseStatus
sendMailInSES fromAddress toAddresses mailSubject mailBody = do
    env <- sesEnvironment
    response <- runResourceT $ runAWST env $ send $ sesSendEmail fromAddress toAddresses mailSubject mailBody
    pure $ response ^. sersResponseStatus

sesSendEmail :: MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> SendEmail
sesSendEmail fromAddress toAddresses subjectText bodyText =
    let
        destinationContent = destination & dToAddresses .~ toAddresses
        bodyContent = body & bText ?~ content bodyText
        subjectContent = content subjectText
        messageContent = message subjectContent bodyContent
    in sendEmail fromAddress destinationContent messageContent
