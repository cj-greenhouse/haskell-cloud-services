module Main where
--
import Test.Tasty (defaultMain)
import qualified Spec.CloudServices as CloudServices

main :: IO ()
main = defaultMain CloudServices.tests