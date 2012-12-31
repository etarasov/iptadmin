
module IptAdmin.EditChainForm.Parse where

import Control.Monad
import Data.Char
import Iptables.Parser
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (State(..))

parseChainName :: GenParser Char () String
parseChainName = do
    spaces <?> ""
    name <- chainNameParser `label` "latin letter, digit, '_' or '-'"
    when (all isUpper name) $
        fail "Uppercase letters only is prohibited for chain name"
    spaces
    eof
    return name
