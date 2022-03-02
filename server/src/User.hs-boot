module User where

import qualified Auth.Schema as Auth
import Control.Concurrent.STM.TVar (TVar)
import Data.Text (Text)
import Stats.Experience (Experience)

data User
  = User Auth.User (TVar Experience)
  | CpuUser Text Experience
  | GuestUser Text (TVar Experience)
  | ServiceUser
