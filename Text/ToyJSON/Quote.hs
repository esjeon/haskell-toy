{-# LANGUAGE DeriveDataTypeable #-}

module Text.ToyJSON.Quote (json) where

import Data.Generics
import Data.Maybe
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ

import Text.ToyJSON

json :: THQ.QuasiQuoter
json = THQ.QuasiQuoter { THQ.quoteExp = expr
                       , THQ.quotePat = error "unimplemented"
                       , THQ.quoteType = error "unimplemented"
                       , THQ.quoteDec = error "unimplemented"
                       }

-- NOTE: it is possible to track parser error location.
--       see ToyMath example for more info

expr :: String -> TH.ExpQ
expr s = case parseJSON s of
    Left err -> fail $ show err
    Right e -> (THQ.dataToExpQ (const Nothing `extQ` trans)) e

trans :: JSONValue -> Maybe (TH.Q TH.Exp)
trans (JSONBool v) = Just $ TH.appE (TH.conE (TH.mkName "JSONBool")) 
                                    (TH.conE (TH.mkName "False"))
trans (JSONString s) = Just $ TH.appE (TH.conE (TH.mkName "JSONString")) 
                                      (TH.litE (TH.stringL s))
trans (JSONArray xs) = Just $ TH.appE (TH.conE (TH.mkName "JSONArray")) 
                                      (TH.listE $ map do_value xs)
trans (JSONObject xs) = Just $ TH.appE (TH.conE (TH.mkName "JSONObject")) 
                                       (TH.listE $ map do_pair xs)

do_value :: JSONValue -> TH.Q TH.Exp
do_value = fromJust . trans

do_pair :: (String, JSONValue) -> TH.ExpQ
do_pair (k,v) = TH.tupE [TH.litE (TH.stringL k), do_value v]

