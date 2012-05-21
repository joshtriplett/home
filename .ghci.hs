module GHCI(
    (#)
) where
import Data.Char(intToDigit)
import Numeric(showIntAtBase)
(#) :: (Integral a, Show a) => a -> a -> IO ()
num # base = putStrLn $ showIntAtBase base intToDigit num ""
infix 0 #
