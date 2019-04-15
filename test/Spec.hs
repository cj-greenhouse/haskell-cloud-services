module Main where
--
import Test.Tasty (defaultMain)
import qualified Spec.Effect as Effect

main :: IO ()
main = defaultMain Effect.specs