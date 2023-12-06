import ChessParser
import ChessSyntax
import Control.Monad.State qualified as S
import Lib
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"

-------------------------
-- Arbitrary definitions--
-------------------------
instance Arbitrary Game where
  arbitrary = undefined

instance Arbitrary Move where
  arbitrary = undefined

-- Check if the game board changes after a move
prop_validMove :: Game -> Move -> Property
prop_validMove game move =
  runState (validMove move) game ==>
    runState (playMove move) game /= game
