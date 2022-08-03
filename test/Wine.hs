{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module Wine where

import GHC.Generics (Generic, Generic1)
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.StateMachine
import Data.Kind (Type)
import Data.List (delete)
import Test.QuickCheck (Gen, Property, (===), oneof, quickCheck)
import Test.QuickCheck.Monadic  (monadicIO)

{-

Statement of the riddle: 

You (Player1) and your friend (Player2) are standing on opposite sides of a river, 
there is no bridge and no way to get across. You have a bottle of wine and 
want to share it with your friend. Luckily there is a boatman with his ferry on 
the river who is offering to help, but unforunately for you he is also a drunk and 
will try to steal your wine. He has a wooden chest on the boat which can be locked. 
You have a lock and a key, and your friend has a lock and a key. Your key only works 
with your lock, and likewise for your friend. The boatman is willing to take as many
trips accross as necissary to help, as long as he gets a glass of wine for his work.

Devise a strategy to share the bottle with your friend.

-}


data Player 
  = Player1 
  | Player2
  deriving stock (Eq, Show, Generic)

deriving instance ToExpr Player

swapPlayer :: Player -> Player 
swapPlayer Player1 = Player2
swapPlayer Player2 = Player1

data WineLocation
  = WLWithPlayer Player
  | WLOnFerry
  deriving stock (Eq, Show, Generic)

deriving instance ToExpr WineLocation

data Model (r :: Type -> Type) 
  = Model 
      { locks :: [Player]
      , wineLocation :: WineLocation
      , currentPlayer :: Player
      }
      deriving stock (Eq, Show, Generic)
      
deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel =
  Model
    { locks = []
    , wineLocation = WLWithPlayer Player1
    , currentPlayer = Player1
    }

data Command (r :: Type -> Type)
  = SendFerryAcross 
  | LockLock
  | OpenLock
  | PlaceWine
  | TakeWine
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

data Response (r :: Type -> Type)
  = Done
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Foldable)


--------------------------------------------------------------------------------

preconditions :: Model Symbolic -> Command Symbolic -> Logic
preconditions Model{..} cmd = case cmd of
  SendFerryAcross ->
    if wineLocation == WLOnFerry && locks == []
	then Bot
	else Top
  LockLock -> Boolean (not $ currentPlayer `elem` locks)
  OpenLock -> Boolean (currentPlayer `elem` locks)
  PlaceWine -> 
    Boolean (WLWithPlayer currentPlayer == wineLocation) .&&
    Boolean (locks == [])
  TakeWine -> 
    Boolean (wineLocation == WLOnFerry) .&& 
      Boolean (locks == [])

transitions :: Model r -> Command r -> Response r -> Model r
transitions m@Model{..} cmd _ = 
  case cmd of
    SendFerryAcross -> Model {currentPlayer = swapPlayer currentPlayer, .. }
    LockLock -> Model {locks = currentPlayer : locks, .. }
    OpenLock -> Model {locks = delete currentPlayer locks, ..}
    PlaceWine -> Model {wineLocation = WLOnFerry, ..}
    TakeWine -> Model {wineLocation = WLWithPlayer currentPlayer, ..}

postconditions :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postconditions s c r = 
  let Model{..} = transitions s c r
  in Not $ Boolean (wineLocation == WLWithPlayer Player2)

generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator _ = 
  Just $ oneof $ map pure
    [ SendFerryAcross
    , LockLock
    , OpenLock
    , PlaceWine
    , TakeWine
    ]


shrinker :: Model r -> Command r -> [Command r]
shrinker _ _ = []

semantics :: Command Concrete -> IO (Response Concrete)
semantics _ = return Done

mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock _ _ = return Done

sm :: StateMachine Model Command IO Response
sm = StateMachine initModel transitions preconditions postconditions
       Nothing generator shrinker semantics mock noCleanup

prop_shareWine :: Property
prop_shareWine = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, _model, res) <- runCommands sm cmds
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))