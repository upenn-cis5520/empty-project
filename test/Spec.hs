import Test.HUnit
import Test.QuickCheck

import VideoToImageTest
import ImageIOTest

main :: IO ()
main = do
    _ <- runVideoToImageTests
    _ <- runImageIOTests
    return ()
