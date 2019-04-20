# haskell-aws

Simplifying wrapper for AWS SDK

## Example Wiring

### Object Store

```
import Network.AWS (newEnv, Credentials(Discover))

instance S3Environment IO where
    s3Environment = newEnv Discover

instance ObjectStore <runtime implementation> where
    getObject   = getObjectInS3
    putObject   = putObjectInS3
    listObjects = listObjectsInS3
```

### Mail Exchange

```
import Control.Monad.Trans.AWS (Region(NorthVirginia))
import Network.AWS (newEnv, Credentials(Discover))

instance MailExchange <runtime implementation> where
    sendMail = sendMailInSES

instance SESEnvironment IO where
    sesEnvironment = set envRegion NorthVirginia <$> newEnv Discover
```

## Links

* Amazonka [[Hackage](https://hackage.haskell.org/package/amazonka)] [[GitHub](https://github.com/brendanhay/amazonka)]


