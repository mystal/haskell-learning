-- file: ch07/toupper-lazy4.hs
import Data.Char(toUpper)

-- Just takes a String from stdin, performs some function, and outputs to stdout
main = interact (map toUpper)
