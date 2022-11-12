module ImageIO where

import Data.Matrix
import Codec.Picture (DynamicImage)

data RGBImage = RGBImage
    {
        r :: Matrix Int,
        g :: Matrix Int,
        b :: Matrix Int
    }


{-
Reads the image from the given file path

Args:
    - image file path

Returns:
    - Either
        - error message
        - the RGB Image instance
-}
readImage :: FilePath -> IO (Either String RGBImage)
readImage = undefined


{-
Convert the DynamicImage to RGBImage,
the former one is the type in JuicyPixels which may be unintuitive to manipulate,
and the latter is just three matrices of integers.

Args:
    - the DynamicImage

Returns:
    - the RGBImage
-}
dynamicImageToRGBImage :: DynamicImage -> RGBImage
dynamicImageToRGBImage = undefined

