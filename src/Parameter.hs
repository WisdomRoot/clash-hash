module Parameter where

import Clash.Explicit.Prelude

data MLKEM = MLKEM512 | MLKEM768 | MLKEM1024 deriving (Eq, Show)