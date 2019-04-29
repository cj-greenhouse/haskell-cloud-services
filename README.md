# haskell-cloud-services

Simplifying wrapper for AWS SDKs

## Example Wirings

### Mail Exchange

```
import CloudServices     (MailExchange (..))
import CloudServices.AWS (SESEnvironment, northVirginiaDefaultEnvironment)

instance MailExchange <runtime implementation> where
    sendMail = sendMailInSES

instance SESEnvironment IO where
    sesEnvironment = northVirginiaDefaultEnvironment
```
### MessageQueue

```
import CloudServices     (MessageQueue (..))
import CloudServices.AWS (SQSEnvironment, defaultEnvironment)

instance MessageQueue <runtime implementation> where
    sendMessage     = sendMessageInSQS
    receiveMessages = receiveMessagesInSQS
    deleteMessage   = deleteMessageInSQS

instance SQSEnvironment IO where
    sqsEnvironment  = defaultEnvironment
```

### Object Store

```
import CloudServices      (ObjectStore (..))
import CloudServices.AWS  (S3Environment, defaultEnvironment)

instance ObjectStore <runtime implementation> where
    getObject   = getObjectInS3
    putObject   = putObjectInS3
    listObjects = listObjectsInS3

instance S3Environment IO where
    s3Environment = defaultEnvironment
```


## Links

* Amazonka [[Hackage](https://hackage.haskell.org/package/amazonka)] [[GitHub](https://github.com/brendanhay/amazonka)]


