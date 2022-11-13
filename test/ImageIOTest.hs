module ImageIOTest where



import Test.HUnit
import Test.QuickCheck
import ImageIO
import Codec.Picture
import Data.Matrix
import Control.Monad


prop_RGBImage_valid_range :: DynamicImage -> Bool
prop_RGBImage_valid_range x =
    let
        img = dynamicImageToRGBImage x
        valid = foldr (\elem acc -> acc && 0 <= elem && elem <= 255) True
    in
        all valid [r img, b img, g img]

prop_RGBImage_same_size :: DynamicImage -> Bool
prop_RGBImage_same_size x =
    let
        img = dynamicImageToRGBImage x
        w = dynamicMap imageWidth x
        h = dynamicMap imageHeight x
    in
        all ((== w) . (\ f -> ncols $ f img)) [r, g, b] &&
        all ((== h) . (\ f -> nrows $ f img)) [r, g, b]

instance Arbitrary PixelRGB8 where
    arbitrary :: Gen PixelRGB8
    arbitrary = liftM3 PixelRGB8 arbitrary arbitrary arbitrary

instance Arbitrary (Image PixelRGB8) where
    arbitrary :: Gen (Image PixelRGB8)
    arbitrary = liftM3 generateImage arbitrary (choose (1, 10)) (choose (1, 10))

instance Arbitrary DynamicImage where
    arbitrary :: Gen DynamicImage
    arbitrary = liftM ImageRGB8 arbitrary

instance Show DynamicImage where
    show :: DynamicImage -> String
    show x =
        let
            image = convertRGB8 x
        in
            "DynamicImage of width=" ++ 
            show (imageWidth image) ++ 
            ", height=" ++ 
            show (imageHeight image)

{-
Test for the conversion correctness
-}
test_dynamicImageToRGBImage :: Test
test_dynamicImageToRGBImage =
    let
        image1 = generateImage (
                    \i j -> PixelRGB8 (fromIntegral i) (fromIntegral j) (fromIntegral $ i + j)
                ) 3 4
        expected1 = RGBImage {
            r = matrix 3 4 fst,
            g = matrix 3 4 snd,
            b = matrix 3 4 $ uncurry (+)
        }

        image2 = generateImage (
                    \i j -> PixelRGB8 (fromIntegral i + 1) (fromIntegral j + 1) (fromIntegral $ i * j)
                ) 3 2
        expected2 = RGBImage {
            r = matrix 3 2 $ (+1) . fst,
            g = matrix 3 2 $ (+1) . snd,
            b = matrix 3 2 $ uncurry (*)
        }
    in
        TestList [
            dynamicImageToRGBImage (ImageRGB8 image1) ~?= expected1,
            dynamicImageToRGBImage (ImageRGB8 image2) ~?= expected2
        ]


runImageIOTests :: IO ()
runImageIOTests =
    do
        quickCheck prop_RGBImage_same_size
        quickCheck prop_RGBImage_valid_range
        runTestTT test_dynamicImageToRGBImage
        return ()

