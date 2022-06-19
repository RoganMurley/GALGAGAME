module User where

import qualified Auth.Schema as Auth
import Control.Concurrent.STM.TVar (TVar)
import Data.Text (Text)
import Stats.Progress (Progress)

data User
  = User Auth.User (TVar Progress)
  | CpuUser Text Progress
  | GuestUser Text (TVar Progress)
  | ServiceUser
