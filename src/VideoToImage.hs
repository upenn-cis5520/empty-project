{-
This module contains all the auxiliary function to invoke ffmpeg
and convert the target video to png files, saved in a temporary working directory.
-}

module VideoToImage where

import System.Process
import System.FilePath

import qualified Variables


{-
Generate the images from the video at the temporary directory.

Args:
    - file path of the video
    - fps

Returns:
    - Either
        - error message
        - (), unit representing success
-}
generateImages :: FilePath -> Int -> IO (Either String ())
generateImages = undefined



{-
Generate the command to execute for video to image generation

Args:
    - file path of the video
    - FPS

Returns:
    - Tuple of the arguments that are ready to be passed in `readProcessWithExitCode`
-}
getFFmpegCommand :: FilePath -> Int -> (String, [String], String)
getFFmpegCommand = undefined
