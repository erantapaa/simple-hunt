module Hunt.Common.BasicTypes
where

import           Data.Text
import qualified Hunt.Common.DocDesc as DD

type Context = Text
type Content = Text
type URI     = Text
type Description = DD.DocDesc
type RegEx = String

