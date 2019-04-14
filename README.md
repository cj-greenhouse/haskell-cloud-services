# amazonka-wrapper

Simplifying wrapper for Amazonka's AWS SDK, opinionated towards default environments and type classes implementations.

## Examples

Example effect wiring using provided type classes and implementations

```
type Runtime = ...

instance ObjectStore Runtime where
    getObject   = getS3Object
    putObject   = putS3Object
    listObjects = listS3Objects
```

## Reference

* Amazonka: Comprehensive Amazon Web Services SDK
    * [Hackage](https://hackage.haskell.org/package/amazonka)
    * [GitHub](https://github.com/brendanhay/amazonka)


