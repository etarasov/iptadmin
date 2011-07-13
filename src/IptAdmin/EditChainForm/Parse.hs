
module IptAdmin.EditChainForm.Parse where

import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (State(..))

-- let's assume that chain names can contain only letters and digits
-- it's not mentioned in iptables man page
parseChainName :: GenParser Char () String
parseChainName = do
    spaces <?> ""
    name <- many1 $ oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) `label` "latin letter of digit"
    when (all isUpper name) $
        fail "Uppercase letters only is prohibited for chain name"
    spaces
    eof
    return name
