module Spec.CloudServices (tests) where
--
import Test.Tasty (TestTree, testGroup)

import qualified Spec.CloudServices.AWS as AWS
import qualified Spec.CloudServices.KeyValueStore as KeyValueStore (tests)
import qualified Spec.CloudServices.MailExchange as MailExchange (tests)
import qualified Spec.CloudServices.MessageQueue as MessageQueue (tests)
import qualified Spec.CloudServices.ObjectStore as ObjectStore (tests)
import qualified Spec.CloudServices.Workflow as Workflow (tests)

tests :: TestTree
tests = testGroup "Cloud Services" [
    AWS.tests,
    KeyValueStore.tests,
    MailExchange.tests,
    MessageQueue.tests,
    ObjectStore.tests,
    Workflow.tests
 ]