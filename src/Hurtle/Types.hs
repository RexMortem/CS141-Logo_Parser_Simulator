module Hurtle.Types where

import Text.Megaparsec
import Data.Void
import Graphics.Gloss

--------------------------------------------------------------------------------
-- Type Definitions

-- | wrapper types

{-
  We had to declare a wrapper instance for Color because it already 
  has an implementation of Show but it is marked with [Safe] and so can't be 
  used in a data structure which derives Show because it can't be converted 
  into a String for every instance of Color.

  Basically, show was not working for Color so we made this wrapper so that we 
  could make show work with the wrapper to store the data without problems. 
-}
newtype WrappedColor = WrapColor {
  unwrapColor :: Color 
} deriving (Show, Eq)

instance Read WrappedColor where -- this can't be automatically derived 
  readsPrec _ _ = undefined 

-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]

data HogoCode
  -- | Movement Commands
  = GoForward Float
  | GoBackward Float
  | TurnLeft Float
  | TurnRight Float
  | GoHome
  -- | Pen Commands
  | PenUp
  | PenDown
  | ClearScreen
  | Colour WrappedColor -- additional
  -- | PenWidth Int -- additional 
  -- | Control Flow/ Block Actions 
  | Repeat Int HogoProgram
  deriving (Show,Read,Eq)

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String

-- | Simulation State Stuff 

data PenState = PenState {
    active :: Bool, 
    colour :: WrappedColor
    -- width :: Int 
} deriving Show 

-- | contains penState for information about drawing between lines as well as position
data PathPoint = PathPoint {
    pathPenState :: PenState, 
    position :: Point, 
    pathTurtleAngle :: Float,
    instantaneous :: Bool, -- for gohome and clearscreen, the movement should be instant 
    clearing :: Bool -- for clearscreen, we should clear lines at this point 
} deriving Show 

data Simulation = Simulation {
    turtlePosition :: Point, 
    turtleAngle :: Float, 
    penState :: PenState,
    path :: [PathPoint]
} deriving Show 