module TypeClasses where

import Data.Text

class IsText a where
  getText :: a -> Text