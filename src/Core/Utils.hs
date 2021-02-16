module Core.Utils where

import Data.Aeson (Value)
import qualified Data.Aeson.Extra.Merge as M
import Data.HashMap.Strict as HM

-- Aeson Value merge (left-bias)
merge :: Value -> Value -> Value
merge = M.merge f
  where f r (M.ObjectF a) (M.ObjectF b) = M.ObjectF $ HM.unionWith r a b
        f _ (M.ArrayF a)  (M.ArrayF b)  = M.ArrayF $ a <> b
        f _ a _                         = a
