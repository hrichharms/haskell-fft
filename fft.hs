{-# LANGUAGE TupleSections #-}


import Data.Complex


evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _ = []


odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []


twiddle :: Bool -> (Int, Int) -> Complex Double
twiddle False (k, n) = cos phase :+ sin phase
    where phase = -2.0 * pi * fromIntegral k / fromIntegral n
twiddle True (k, n) = cos phase :+ sin phase
    where phase = 2.0 * pi * fromIntegral k / fromIntegral n


fftTwiddles :: Int -> Bool -> [Complex Double]
fftTwiddles n inverse = map (twiddle inverse . (, n)) [0..n-1]


r2Butterfly :: [Complex Double] -> [Complex Double] -> [Complex Double] -> Int -> Int -> [Complex Double]
r2Butterfly twiddles even_dft odd_dft n s = map butterfly1 [0..n-1] ++ map butterfly2 [0..n-1]
    where butterfly1 k = (even_dft !! k) + ((twiddles !! (k * s)) * (odd_dft !! k))
          butterfly2 k = (even_dft !! k) - ((twiddles !! (k * s)) * (odd_dft !! k))


fftRecursive :: [Complex Double] -> [Complex Double] -> Int -> Int -> [Complex Double]
fftRecursive twiddles input 1 s = input
fftRecursive twiddles input n s = r2Butterfly twiddles even_dft odd_dft (div n 2) s
    where even_dft = fftRecursive twiddles (evens input) (div n 2) (2 * s)
          odd_dft = fftRecursive twiddles (odds input) (div n 2) (2 * s)


fft :: [Complex Double] -> [Complex Double] -> Bool -> [Complex Double]
fft twiddles input False = fftRecursive twiddles input (length twiddles) 1
fft twiddles input True = map (\x -> x / fromIntegral n) (fftRecursive twiddles input n 1)
    where n = length twiddles


main :: IO()
main = do

    let forward_twiddles = fftTwiddles 4 False
    let inverse_twiddles = fftTwiddles 4 True


    let input = [1.0 :+ (-1.0), 0.0 :+ 81.0, 1.0 :+ 0.0, (-1.0) :+ 12.0]

    print( fft forward_twiddles input False )
    print( fft inverse_twiddles (fft forward_twiddles input False) True)
