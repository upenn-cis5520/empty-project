{-
This module defines several shared variables, such as the path for
temporary image files, or the ascii video content file.
-}

module Variables where

import System.FilePath ( (<.>), (</>) )



{-
Temporary directory for storing all the intermediate files.
-}
tempDir :: FilePath
tempDir = ".ascii-vid-generator-temp"

{-
Path of the video content file.
-}
contentFile :: FilePath
contentFile = tempDir </> "video-content"


{-
Getter of the path of image file.

Args:
    - the index of the image. Note that this is just a formatter to produce
      the file path. It does not check if such file exists.
-}
imageFile :: Int -> FilePath
imageFile n = tempDir </> show n <.> "png"


{-
Path format argument that should be passed to FFmpeg command, which corresponds
to the imageFile format defined above.
-}
imageFileFFmpegFormat :: String
imageFileFFmpegFormat = tempDir </> "%d" <.> "png"
