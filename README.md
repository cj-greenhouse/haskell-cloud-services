# haskell-cloud-services

Simplifying wrapper for AWS SDKs

## Example Wirings

### Mail Exchange

```
import MailExchange
import AWS.Environment (northVirginiaDefaultEnvironment)

instance MailExchange <runtime implementation> where
    sendMail = sendMailInSES

instance SESEnvironment IO where
    sesEnvironment = northVirginiaDefaultEnvironment
```
### MessageQueue

```
import MessageQueue
import AWS.Environment (defaultEnvironment)

instance MailExchange <runtime implementation> where
    sendMail = sendMailInSES

instance SESEnvironment IO where
    sendMessage = sendMessageInSQS
    receiveMessages = receiveMessagesInSQS
    deleteMessage = deleteMessageInSQS
```

### Object Store

```
import ObjectStore
import AWS.Environment (defaultEnvironment)

instance S3Environment IO where
    s3Environment = defaultEnvironment

instance ObjectStore <runtime implementation> where
    getObject   = getObjectInS3
    putObject   = putObjectInS3
    listObjects = listObjectsInS3
```


## Links

* Amazonka [[Hackage](https://hackage.haskell.org/package/amazonka)] [[GitHub](https://github.com/brendanhay/amazonka)]


