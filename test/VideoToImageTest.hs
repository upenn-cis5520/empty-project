module VideoToImageTest where

import Test.HUnit
import Test.QuickCheck
import VideoToImage (getFFmpegCommand)
import Variables (imageFileFFmpegFormat)


test_getFFmpegCommand :: Test
test_getFFmpegCommand =
    TestList [
        getFFmpegCommand "video.mp4" 10 ~?=
            ("ffmpeg", ["-i", "video.mp4", "-vf", "fps=10", imageFileFFmpegFormat], ""),

        getFFmpegCommand "video.flv" 1 ~?=
            ("ffmpeg", ["-i", "video.flv", "-vf", "fps=1", imageFileFFmpegFormat], "")
    ]

runVideoToImageTests :: IO Counts
runVideoToImageTests = runTestTT $
    TestList [
        test_getFFmpegCommand
    ]
