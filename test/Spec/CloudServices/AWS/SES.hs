module Spec.CloudServices.AWS.SES where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.Helper ((===))

tests :: TestTree
tests = testGroup "SES" []