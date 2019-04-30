module Spec.CloudServices.AWS.DynamoDB where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.Helper ((===))

tests :: TestTree
tests = testGroup "Dynamo DB" []