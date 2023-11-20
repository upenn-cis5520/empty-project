import Test.HUnit
import Test.QuickCheck
import qualified LuStepper as LS
import qualified LuParser  as LP
import qualified LuParserTest as P

main :: IO ()
main = do
    putStrLn "*** Testing LuParser ***"
    P.test
    P.qc
    putStrLn "*** Testing ... ***"
--   putStrLn "*** Testing LuStepper ***"
--   LS.test_all -- unit tests
--   LS.qc       -- quickcheck properties
--   putStrLn "*** Testing LuParser ***"
--   LP.test_all -- unit tests
--   LP.qc       -- quickcheck properties
