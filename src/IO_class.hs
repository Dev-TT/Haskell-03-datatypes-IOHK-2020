
module IO_class where
--07-IO CLASS 050

-- We hide functions we are going to redefine.
import Prelude hiding ( or, reverse, filter)
--import Control.Applicative ((<|>), liftA2)

import Control.Monad (liftM, liftM2)
import Data.Char (toUpper)
--import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
--import System.IO.Error (catchIOError)


getTwoLines :: IO String
getTwoLines = getLine >> getLine

duplicateLine :: IO String
duplicateLine = liftM (\ x -> x ++ x) getLine

-- toUpper 'x'
-- X

-- Shouting: 
-- liftM (map toUpper) getLine

joinTwoLines :: IO String
joinTwoLines = liftM2 (++) getLine getLine




shout :: IO String
shout = liftM (map toUpper) getLine





--END 07-IO Class
