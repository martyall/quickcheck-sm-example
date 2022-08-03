{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module Resource where

import GHC.Generics (Generic, Generic1)
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.StateMachine
import Data.Kind (Type)
import Test.QuickCheck (Gen, Property, (===), oneof, quickCheck)
import Test.QuickCheck.Monadic  (monadicIO)
