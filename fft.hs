{-# LANGUAGE TupleSections #-}


import Data.Complex


evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _ = []


odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []


twiddle :: (Int, Int) -> Complex Double
twiddle (k, n) = cos phase :+ sin phase
    where phase = -2.0 * pi * fromIntegral k / fromIntegral n


fftTwiddles :: Int -> [Complex Double]
fftTwiddles n = map (twiddle . (, n)) [0..n-1]


r2Butterfly :: [Complex Double] -> [Complex Double] -> [Complex Double] -> Int -> [Complex Double]
r2Butterfly twiddles even_dft odd_dft n = map butterfly1 [0..n-1] ++ map butterfly2 [0..n-1]
    where butterfly1 k = (even_dft !! k) + (twiddle (k, 2 * n) * (odd_dft !! k))
          butterfly2 k = (even_dft !! k) - (twiddle (k, 2 * n) * (odd_dft !! k))


fft :: [Complex Double] -> [Complex Double] -> Int -> [Complex Double]
fft twiddles input 1 = input
fft twiddles input n = r2Butterfly twiddles even_dft odd_dft (div n 2)
    where even_dft = fft twiddles (evens input) (div n 2)
          odd_dft = fft twiddles (odds input) (div n 2)


main :: IO()
main = do

    let twiddles = fftTwiddles 8


    let input = [1.0 :+ (-1.0), 0.0 :+ 81.0, 1.0 :+ 0.0, (-1.0) :+ 12.0, 1.0 :+ (-1.0), 0.0 :+ 83.0, 1.0 :+ 0.0, (-1.0) :+ (-122.0)]

    print( fft twiddles input 8 )
