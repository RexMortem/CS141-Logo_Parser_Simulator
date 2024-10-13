module Hurtle.SimulationState where 

import Control.Monad.State
import Graphics.Gloss

-- Our Modules 
import Hurtle.Types 

{-
    Takes a HogoProgram and converts it into a stateful computation 
    Note that the stateful computation's return value does not matter so it is unit ()
    This is because we only care about the state of the simulation
    In particular, the final state (after fully executing)

    We does this through a composition of functions
    We know executeActions :: [State Simulation ()] -> State Simulation ()
    and hogoProgramToSimulationActions :: HogoProgram -> [State Simulation ()]

    The types match and their meanings match:
    hogoProgramToSimulationActions first converts the program to a list of computations.
    This list of computations is then piped to executeActions which applies them all.
-}
runSimulation :: HogoProgram -> State Simulation ()
runSimulation = executeActions.hogoProgramToSimulationActions

{-
    Standard penstate 
    Used in the initial state 
-}
standardPenState :: PenState 
standardPenState = PenState {
    active = True, 
    colour = WrapColor black
    -- width = 5
}

{-
    Standard origin point
    The turtle always starts at (0,0) with angle 90 (facing up/North)
-}
initialPoint :: PathPoint 
initialPoint = PathPoint {
    position = (0,0),
    instantaneous = True,
    clearing = False,
    pathTurtleAngle = 90,
    -- All these other details don't actually matter since they aren't drawn 
    pathPenState = standardPenState
}

{-
    Standard initial state
    Starts at (0,0) with angle 90 (facing up/North)
-}
initialState :: Simulation 
initialState = Simulation {
    turtlePosition = (0, 0),
    turtleAngle = 90, -- 90 is facing up
    penState = standardPenState,
    path = [initialPoint]
}

{-
    Point-free function which just uses sequence_ to apply all computations in left-to-right order
-}
executeActions :: [State Simulation ()] -> State Simulation ()
executeActions = sequence_ 

{-
    Maps over HogoProgram to convert each HogoCode inside of HogoProgram into a computation 
    The computation will be of type State Simulation ()
    hogoProgramToSimulateActions returns a list of these computations 
-}
hogoProgramToSimulationActions :: HogoProgram -> [State Simulation ()]
hogoProgramToSimulationActions = map modifySimulation 

{-
    Converts an individual Hogo action into a computation
    Does this by using the state monad's modify function composed with updateSimulation. 
    updateSimulation, given a code, becomes a partially applied function of type Simulation -> Simulation. 
    
    Modify then takes this function which describes how to convert from a simulation to another simulation 
    and converts it into a computation. 
-}
modifySimulation :: HogoCode -> State Simulation ()
modifySimulation = modify.updateSimulation 

{-
    A huge function that describes how to update a simulation given an individual action. 
    Does this by pattern matching on every Hogo action and returns the updated simulation. 
-}
updateSimulation :: HogoCode -> Simulation -> Simulation 
--no argument actions
updateSimulation PenDown simulation = let oldPenState = penState simulation in simulation {penState = oldPenState { active = True }} 
updateSimulation PenUp simulation = let oldPenState = penState simulation in simulation {penState = oldPenState { active = False }} 
updateSimulation ClearScreen simulation = resetToHome True simulation
updateSimulation GoHome simulation = resetToHome False simulation 
-- single argument actions
updateSimulation (GoForward distance) simulation = let 
        distanceVector :: Point 
        distanceVector = getDistanceVector distance simulation 
        
        newPoint :: Point -- positively applies the distance vector to get new point 
        newPoint = (fst (turtlePosition simulation) + fst distanceVector, snd (turtlePosition simulation) + snd distanceVector)

        simulationWithNewPath :: Simulation 
        simulationWithNewPath = addPathPoint newPoint False False simulation 
    in 
        simulationWithNewPath {
            turtlePosition = newPoint
        }

updateSimulation (GoBackward distance) simulation = let 
        distanceVector :: Point 
        distanceVector = getDistanceVector distance simulation 
        
        newPoint :: Point -- negatively applies the distance vector to get new point 
        newPoint = (fst (turtlePosition simulation) - fst distanceVector, snd (turtlePosition simulation) - snd distanceVector)

        simulationWithNewPath :: Simulation 
        simulationWithNewPath = addPathPoint newPoint False False simulation 
    in 
        simulationWithNewPath {
            turtlePosition = newPoint
        }

-- angles are defined counter-clockwise from the x basis vector, so left is + and right is - 
updateSimulation (TurnLeft angleInDeg) simulation = simulation {turtleAngle = turtleAngle simulation + angleInDeg}
updateSimulation (TurnRight angleInDeg) simulation = simulation {turtleAngle = turtleAngle simulation - angleInDeg}

-- block actions 
updateSimulation (Repeat n actions) simulation = foldlSimulationNTimes n updateSimulation simulation actions

-- extension actions 
updateSimulation (Colour color) simulation = let oldPenState = penState simulation in simulation {penState = oldPenState { colour = color }} 
--updateSimulation (PenWidth width) simulation = let oldPenState = penState simulation in simulation {penState = oldPenState { width = width }} 

-- helper functions

{-
    resetToHome is a helper function that adds a new point sending the turtle
    immediately backt to home (hence the True which is instantaneous :: Bool)

    May having clearing set to True depending on if this is called from clearscreen or not 

    Uses a let binding to clear up the function 
-}
resetToHome :: Bool -> Simulation -> Simulation 
resetToHome clearing simulation = let
        simulationWithNewPath :: Simulation 
        simulationWithNewPath = addPathPoint (0,0) True clearing simulation
    in 
        simulationWithNewPath {
            turtlePosition = (0, 0),
            turtleAngle = 90
        }
{-
    Unfortunately, foldl's type definition does not fit our updateSimulation function. 
    So we have to write foldl in terms of the order of arguments of updateSimulation. 

    It is a recursive function where the cases of an empty list of hogocode actions is considered,
    and a list of a single hogocode action is considered using top-level pattern matching. 

    For an empty list of actions, we just want to return the simulation how it was. 
    For a single action, we just want to apply that single action and then return the new simulation. 

    The recursive case (2+ actions) recurses down to the base case of 1 hogocode action.
    In action:otherActions, action is the first element of the list.
    Hence, we want to apply action first (since it's foldl) then all the others so we recurse. 

    ==//==

    Alternatively, we could've redefined updateSimulation to swap the arguments to fit foldl's type definition:

    updateSimulationForFolding :: Simulation -> HogoCode -> Simulation 
    updateSimulationForFolding sim code = updateSimulation code sim

    However, I thought it showed more flair to write a custom foldl. 
-}

foldlSimulation :: (HogoCode -> Simulation -> Simulation) -> Simulation -> HogoProgram -> Simulation 
foldlSimulation _ simulation [] = simulation                                                   
foldlSimulation f simulation [action] = f action simulation -- base case for the recursive case (+ single case)    
foldlSimulation f simulation (action:otherActions) = foldlSimulation f (f action simulation) otherActions

{-
    foldlSimulationNTimes uses foldlSimulation n times 
    It has a base case of 0 applications
    To foldl n times, it just returns the simulation untouched

    In the recursive case, we use a let binding to call foldlSimulation to foldl once.
    We then call the same function but with n times reduced so that it applies foldlSimulation n-1 times more. 
-}
foldlSimulationNTimes :: Int -> (HogoCode -> Simulation -> Simulation) -> Simulation -> HogoProgram -> Simulation 
foldlSimulationNTimes 0 _ simulation _ = simulation -- base case
foldlSimulationNTimes n f simulation list = let  -- recursive case
                                          transformedSimulation :: Simulation 
                                          transformedSimulation = foldlSimulation f simulation list 
                                      in
                                          foldlSimulationNTimes (n-1) f transformedSimulation list 

{-
    For when we need to get head in a safe way 
    Uses top-level pattern matching to return either nothing or just the value 
    This can then be pattern matched by a function using safeHead without worrying about errors 
-}
safeHead :: [a] -> Maybe a 
safeHead [] = Nothing 
safeHead (x:_) = Just x 

{-

    ==/==

    I want to clarify that safeHead is now redundant but was used for the version of this function 
    that, after the in, instead of being written:

    simulation {
        path = pointToAdd : path simulation -- reverse order, but we'll draw in reverse 
    }

    It was written:

    if active currentPenState then 
        -- pen is on; always add point 
        simulation {
            path = pointToAdd : path simulation -- reverse order, but we'll draw in reverse 
        }
    else 
        -- there is a chance we're in a string of invisible points; don't record redundant data 
        -- let's check the previous point; we'll remove it if it's invisible  
        case safeHead $ path simulation of {
            -- no previous point
            Nothing -> simulation {
                path = pointToAdd : path simulation
            };

            -- there was a previous point; if it wasn't active, then it's redundant 
            (Just previousPoint) -> 
                if active $ pathPenState previousPoint then 
                    -- it was active; therefore, a line is drawn to it 
                    simulation {
                        path = pointToAdd : path simulation
                    }
                else 
                    -- it was inactive! It's a redundant point. Let's remove head by taking tail 
                    simulation {
                        path = pointToAdd : (tail $ path simulation)
                    }
        }

    THIS was actually an optimisation at the time, as it removed redundant points.
    I realised later when I had to animate the turtle that the points were not redundant for the turtle moving.
    They were only redundant for the line drawing. 

    It still is an optimisation if you only care about the line drawing. 
-}
addPathPoint :: Point -> Bool -> Bool -> Simulation -> Simulation 
addPathPoint newPoint instant clearing simulation = let
            currentPenState :: PenState
            currentPenState = penState simulation 

            pointToAdd :: PathPoint 
            pointToAdd = PathPoint {
                position = newPoint, 
                pathTurtleAngle = turtleAngle simulation,
                pathPenState = currentPenState,
                instantaneous = instant,
                clearing = clearing
            }
        in 
            simulation {
                path = pointToAdd : path simulation -- reverse order, but we'll draw in reverse 
            }
            

{-
    Point-free function via partially applying multiplication to (pi/180).
    The other argument to the multiplication will be the angle in degrees that you pass in. 
    Multiplication is converted via () from an infix operator to a function. 
    We use the low precendence function application ($) function so that pi/180 gets calculated first, 
    instead of pi, /, 180 being treated as arguments to (*). 
-}
degToRadians :: Float -> Float 
degToRadians = (*) $ pi/180

{-
    getUnitVector, given an angle, gives the unit vector (of length 1) for that angle
    It does this by converting the angle to radians (since the trig functions use radians)
    and x = cos(theta)
        y = sin(theta)

    Use a let binding to clear the function up

    This works because of the unit circle and we know it's of length 1 because:
    x^2 + y^2 = length 
    cos^2(theta) + sin^2(theta) = 1 

    so after substitution, we know length = 1 
-}
getUnitVector :: Float -> Point
getUnitVector deg = let 
                        radians :: Float 
                        radians = degToRadians deg 
                    in 
                        (cos radians, sin radians)

{-
    getDistanceVector gets the movement vector for the simulation
    This vector can either be applied positively or negatively depending on if it's called by forwards or backwards
    
    It calculates this vector using the unit vector and scales it based on the distance travelled 
    This scalar is accessed using accessor functions 
-}
getDistanceVector :: Float -> Simulation -> Point
getDistanceVector distance sim = let 
                            unitVector :: Point 
                            unitVector = getUnitVector $ turtleAngle sim 
                        in 
                            (distance * fst unitVector, distance * snd unitVector)