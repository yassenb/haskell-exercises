import Data.List
import qualified Data.ByteString as ByteString

-- requires the ppm and netpbm packages installed
import Graphics.Netpbm (parsePPM, PPM(..), PPMHeader(..), pixelDataToIntList)
import Codec.Image.PPM (ppm)

type Pixel = (Int, Int, Int)

red, green, blue :: Pixel -> Int
red (r, _, _) = r
green (_, g, _) = g
blue (_, _, b) = b

inBounds :: (Num a, Ord a) => a -> a
inBounds x | x < 0 = 0
           | x > 255 = 255
           | otherwise = x

type Matrix a = [[a]]

nrows, ncolumns :: Matrix a -> Int
nrows = length
ncolumns = length . head

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap = map . map

-- produces slices of windowSize from a list with each slice being offset to the previous by step
-- for example slice 3 2 [1..8] = [[1,2,3],[3,4,5],[5,6,7],[7,8]]
slice :: Int -> Int -> [a] -> [[a]]
slice windowSize step xs = take windowSize xs :
    if null $ drop windowSize xs then
        []
    else
        slice windowSize step $ drop step xs

split n = slice n n

convolve :: RealFrac a => Matrix a -> Matrix Pixel -> Matrix Pixel
convolve kernel = matrixMap newValue . matrices where
    matrices :: Matrix Pixel -> Matrix (Matrix Pixel)
    -- note that a very magical line follows
    matrices = map transpose . slice (nrows kernel) 1 . map (slice (ncolumns kernel) 1) . withPadding where
        withPadding = map pad . pad where
            pad xs = [head xs] ++ xs ++ [last xs]
    newValue matrix = (newChannelValue red, newChannelValue green, newChannelValue blue) where
        newChannelValue channel = inBounds $ applyKernel $ map channel $ concat matrix where
            applyKernel = round . sum . zipWith (*) (concat kernel) . map fromIntegral

-- TODO not all formats read by Netpbm are trilplets of ints
convert :: RealFrac a => Matrix a -> PPM -> String
convert kernel (PPM PPMHeader { ppmWidth = width } ppmData) = ppm $ convolve kernel colorArray where
    colorArray = split width rgbPixelList
    rgbPixelList = map toTripplet $ split 3 pixelList where
        toTripplet [x1, x2, x3] = (x1, x2, x3)
        pixelList = pixelDataToIntList ppmData

edge, sharpen :: Matrix Double
edge = [[0,  1, 0],
        [1, -4, 1],
        [0,  1, 0]]
sharpen = [[-1, -1, -1],
           [-1,  9, -1],
           [-1, -1, -1]]

main = do
    contents <- ByteString.getContents
    case parsePPM contents of
        Left e -> error "Unable to parse"
        Right (images, _) -> putStr $ convert edge $ head images
