module Multiline where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char(chr)

ml :: QuasiQuoter
ml = QuasiQuoter {
        quoteExp = litE . stringL. filter (/= chr 13),
        quotePat = undefined,
        quoteDec = undefined,
        quoteType = undefined
    }
