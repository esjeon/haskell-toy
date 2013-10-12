{-# LANGUAGE QuasiQuotes #-}
module Text.ToyMath.Example where

import Text.ToyMath.Quote
import Text.ToyMath.Expr
import Text.Printf

eval' :: Expr -> Integer
    -- IntExpr
eval' [expr|$int:x|] = x 
    -- BinopExpr
eval' [expr|$x + $y|] = eval' x + eval' y
eval' [expr|$x - $y|] = eval' x - eval' y
eval' [expr|$x * $y|] = eval' x * eval' y
eval' [expr|$x / $y|] = eval' x `div` eval' y

example1 :: IO ()
example1 = do
    let e = [expr| 1 + 10 |]
    let n1 = eval e
    let n2 = eval' e
    printf $ "eval  ==> " ++ show n1 ++ "\n"
    printf $ "eval' ==> " ++ show n1 ++ "\n"

