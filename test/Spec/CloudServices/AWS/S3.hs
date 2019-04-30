module Spec.CloudServices.AWS.S3 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.Helper ((===))

tests :: TestTree
tests = testGroup "S3" []