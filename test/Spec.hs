import Test.HUnit
import Test.QuickCheck
-- import qualified LuStepper as LS
-- import qualified LuParser  as LP
import qualified LuParserTest as LP
import qualified LuEvaluatorTest as LE
import qualified LuStepperTest as LS

main :: IO ()
main = do
    putStrLn "*** Testing LuParser ***"
    LP.test
    LP.qc
    putStrLn "*** Testing LuEvaluator ***"
    LE.test 
    LE.qc
    putStrLn "*** Testing LuStepper ***"
    LS.test 
    LS.qc
    putStrLn "*** Done Testing ***"

    --LE.qc
--   putStrLn "*** Testing LuStepper ***"
--   LS.test_all -- unit tests
--   LS.qc       -- quickcheck properties
--   putStrLn "*** Testing LuParser ***"
--   LP.test_all -- unit tests
--   LP.qc       -- quickcheck properties
