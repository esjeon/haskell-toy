module Main where

import Expr.Quote

eval' :: Expr -> Integer
    -- AntiIntExpr
eval' [expr|$int:x|] = x 
    -- BinopExpr
eval' [expr|$x + $y|] = eval' x + eval' y
eval' [expr|$x - $y|] = eval' x - eval' y
eval' [expr|$x * $y|] = eval' x * eval' y
eval' [expr|$x / $y|] = eval' x `div` eval' y

main :: IO ()
main = do
    let n1 = eval [expr| 1 + 10 |]
    let 
    print $ show n1

