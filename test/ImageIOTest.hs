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

        allEqual :: Eq a => [a] -> Bool
        allEqual [] = True
        allEqual (x: xs) = all (== x) xs
    in
        allEqual (map nrows [r img, g img, b img])
        && allEqual (map ncols [r img, g img, b img])

instance Arbitrary PixelRGB8 where
    arbitrary :: Gen PixelRGB8
    arbitrary = liftM3 PixelRGB8 arbitrary arbitrary arbitrary

instance Arbitrary (Image PixelRGB8) where
    arbitrary :: Gen (Image PixelRGB8)
    arbitrary = liftM3 generateImage arbitrary (choose (100, 300)) (choose (100, 300))

instance Arbitrary DynamicImage where
    arbitrary :: Gen DynamicImage
    arbitrary = liftM ImageRGB8 arbitrary

instance Show DynamicImage where
    show :: DynamicImage -> String
    show x = 
        let
            image = convertRGB8 x
        in
            "DynamicImage of width=" ++ show (imageWidth image) ++ ", height=" ++ show (imageHeight image)

runImageIOTests :: IO ()
runImageIOTests = 
    do
        quickCheck prop_RGBImage_same_size
        quickCheck prop_RGBImage_valid_range
    
