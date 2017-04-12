module Main where

import Lib
import qualified PizzaFactory as PF
import qualified Matrix
import Len

m = Matrix.initMatrix 10 10

m1 = [[7.0,9.0,8.0],[0.0,9.0,0.0],[4.0,9.0,8.0],[1.0,4.0,8.0]]

m3 = Matrix.gaussianElimination m

main :: IO ()
main = do
        putStrLn  (Matrix.showMatrix m)
        putStrLn "\n"
        putStrLn (Matrix.showMatrix m3)
        


