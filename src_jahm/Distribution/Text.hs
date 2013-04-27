-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Text
-- Copyright   :  Duncan Coutts 2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'Text' class which is a bit like the 'Read' and 'Show'
-- classes. The difference is that is uses a modern pretty printer and parser
-- system and the format is not expected to be Haskell concrete syntax but
-- rather the external human readable representation used by Cabal.
--
module Distribution.Text (
  Text(..),
  display,
  ) where

import qualified Text.PrettyPrint          as Disp

import Data.Version (Version(Version))
import qualified Data.Char as Char (isDigit, isAlphaNum, isSpace)

class Text a where
  disp  :: a -> Disp.Doc

display :: Text a => a -> String
display = Disp.renderStyle style . disp
  where style = Disp.Style {
          Disp.mode            = Disp.PageMode,
          Disp.lineLength      = 79,
          Disp.ribbonsPerLine  = 1.0
        }

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  disp  = Disp.text . show

instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))
