module Literal
  ( literalEnv
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Environment ( getEnv )

quoteEnv :: QuasiQuoter -> QuasiQuoter
quoteEnv QuasiQuoter { quoteExp = qe, quotePat = qp, quoteType = qt, quoteDec = qd } =
  QuasiQuoter { quoteExp = get qe, quotePat = get qp, quoteType = get qt, quoteDec = get qd }
  where
    get :: (String -> Q a) -> String -> Q a
    get old_quoter env_name = do
      cts <- runIO (getEnv env_name)
      old_quoter cts

literal :: QuasiQuoter
literal = QuasiQuoter { quoteExp = return . LitE . StringL }

literalEnv :: QuasiQuoter
literalEnv = quoteEnv literal
