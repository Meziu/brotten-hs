import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Codec.Picture
import qualified Data.Vector.Storable as V

-- Generates the mandelbrot set out of a rectangular area.
-- Takes as input the topleft corner of the rect, the bottom right corner and the resolution of the grid.
mandelbrotrect :: (Float, Float) -> (Float, Float) -> (Int, Int) -> [[(Float, Float, Int)]]
mandelbrotrect (xs, ys) (xe, ye) (xres, yres) = [[(re, im, mandelbrotdiverges (re, im) 255)
                                        | re <- linspace xs xe xres]
                                        | im <- linspace ys ye yres]

-- Returns the amount of iterations needed before diverging.
-- If it doesn't diverge within the maxiter iterations, returns maxiter.
mandelbrotdiverges :: (Float, Float) -> Int -> Int
mandelbrotdiverges point maxiter = length . takeWhile (check) $ take maxiter $ mandelbrotsuccessionlist point
    where check = (< 4) . squarenormal -- magic "curried" syntax to first get the normal and then check if its less than 4

-- Returns a mandelbrot succession list given a point.
mandelbrotsuccessionlist :: (Float, Float) -> [(Float, Float)]
mandelbrotsuccessionlist (re, im) = mandelbrotsuccession (re, im) (0, 0)

-- Recursive function returning a list containing al z's after the current one (given a base and the current z)
mandelbrotsuccession :: (Float, Float) -> (Float, Float) -> [(Float, Float)]
mandelbrotsuccession (rebase, imbase) (re, im) = zsqr:mandelbrotsuccession (rebase, imbase) zsqr
    where zsqr = (re ^ 2 - im ^ 2 + rebase, 2*re*im + imbase)

-- Returns the square of the norm of a complex number.
-- This is faster than using a sqrt since we can simply check it against 4 instead.
squarenormal :: (Float, Float) -> Float
squarenormal (re, im) = re ^ 2 + im ^ 2

-- Linearly spaced values.
-- This is an helper function to generate the mandelbrot rect.
linspace :: Float -> Float -> Int -> [Float]
linspace a b n = [a + (b - a) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]

-- Printing helper for the mandelbrotrect
printMandelbrot :: [[(Float, Float, Int)]] -> IO ()
printMandelbrot grid = mapM_ putStrLn [ [ charFor (re, im, iters) | (re, im, iters) <- row ] | row <- grid ]
    where charFor (_, _, k)
            | k < 255 = 'x'
            | otherwise = '.'

main :: IO ()
main = do
    args <- getArgs

    case args of
        (xStr:yStr:_) -> do
            let mx = readMaybe xStr :: Maybe Int
                my = readMaybe yStr :: Maybe Int
                x = fromMaybe 120 mx
                y = fromMaybe 40 my
                grid = mandelbrotrect (-2, 1) (1, -1) (x, y)

                -- Generate pixel vector directly using unfoldrN
                totalBytes = x * y * 3
                pixelVec = V.unfoldrN totalBytes unfoldPixel (grid, 0)

                -- Unfold function: stream through the grid, emitting 3 bytes per pixel
                unfoldPixel ([], _) = Nothing
                unfoldPixel ([]:rows, _) = unfoldPixel (rows, 0)
                unfoldPixel ((point@(_,_,iters):cols):rows, n)
                    | n < 3 = Just (v, ((point:cols):rows, n + 1))
                    | otherwise = unfoldPixel (cols:rows, 0)
                    where v = fromIntegral (255 - iters)

                -- Convert matrix
                img = Image x y pixelVec :: Image PixelRGB8

            writePng "mandelbrot.png" img
            putStrLn "Wrote image to \"mandelbrot.png\""

        _ -> do
            putStrLn "Insert 2 numbers as parameters for the x,y resolution of the image."
