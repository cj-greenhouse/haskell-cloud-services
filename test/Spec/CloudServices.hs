module Spec.CloudServices (tests) where
--
import Test.Tasty (TestTree, testGroup)
import qualified Spec.CloudServices.AWS as AWS

tests :: TestTree
tests = testGroup "Cloud Services" [
    AWS.tests
 ]