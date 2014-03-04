module Walker where


import Parser
import Data.List


data ParserState a = ParserState a

-- generic tree walking function
class Walker a where
    walk :: (ParserState b -> a -> ParserState b) -> a -> ParserState b
