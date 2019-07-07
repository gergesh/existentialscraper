module Image ( concatImages
             , Image
             , RGB
             , Pixel
             , VS
             , readBS
             , writeImage
             ) where

import Prelude as P
import qualified Data.ByteString as B
import Graphics.Image
import Graphics.Image.IO (writeImage, PNG, JPG)
import Graphics.Image.IO.Formats (PNG, JPG)
import Graphics.Image.Processing (topToBottom)

fromRight :: (Show a) => Either a b -> b
fromRight (Left a) = error $ "fromRight got Left " ++ show a
fromRight (Right b) = b

readBSJPG :: B.ByteString -> Image VS RGB Double
readBSJPG = fromRight . decode JPG

readBSPNG :: B.ByteString -> Image VS RGB Double
readBSPNG = fromRight . decode PNG

readBS :: String -> B.ByteString -> Image VS RGB Double
readBS ".jpg"  = readBSJPG
readBS ".jpeg" = readBSJPG
readBS ".png"  = readBSPNG

concatImages :: (Array arr cs e, Foldable t) => t (Image arr cs e) -> Image arr cs e
concatImages = foldr1 topToBottom
