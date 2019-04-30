module Spec.CloudServices.AWS (tests) where
--
import Test.Tasty (TestTree, testGroup)

import qualified Spec.CloudServices.AWS.DynamoDB as DynamoDB (tests)
import qualified Spec.CloudServices.AWS.Environment as Environment (tests)
import qualified Spec.CloudServices.AWS.S3 as S3 (tests)
import qualified Spec.CloudServices.AWS.SES as SES (tests)
import qualified Spec.CloudServices.AWS.SQS as SQS (tests)

tests :: TestTree
tests = testGroup "AWS" [
    DynamoDB.tests,
    Environment.tests,
    S3.tests,
    SES.tests,
    SQS.tests
 ]