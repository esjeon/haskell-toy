{-# LANGUAGE QuasiQuotes #-}

module Text.ToyJSON.Example where

import Text.ToyJSON
import Text.ToyJSON.Quote
import Text.Printf

example1 :: IO ()
example1 = do
    let res = [json| { "name" : "value", "arr":["Looks", "Okay" ,"to" , "me" ] }  |]
    printf $ show res ++ "\n"

