{-# LANGUAGE DeriveDataTypeable #-}

module Text.ToyJSON.Quote (json) where

import Data.Generics
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ

import Text.ToyJSON

json :: THQ.QuasiQuoter
json = THQ.QuasiQuoter { THQ.quoteExp = expr
                       , THQ.quotePat = pat
                       }

-- NOTE: it is possible to track parser error location.
--       see ToyMath example for more info

expr :: String -> TH.ExpQ
expr s = case parseJSON s of
    Left err -> fail $ show err
    Right e -> (THQ.dataToExpQ (const Nothing)) e

pat :: String -> TH.PatQ
pat s = error "what are you doing..."

