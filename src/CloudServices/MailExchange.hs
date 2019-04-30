module CloudServices.MailExchange where
--
import Data.Text (Text)

type MailToAddresses        = [MailAddress]
type MailFromAddress        = MailAddress
type MailSubject            = Text
type MailBody               = Text
type MailAddress            = Text
type SendMailResponseStatus = Int

class MailExchange m where
    sendMail :: MailFromAddress -> MailToAddresses -> MailSubject -> MailBody -> m SendMailResponseStatus

