module Hurtle.Viewer where 

import Graphics.Gloss
import Graphics.Gloss.Juicy 

-- Our Modules
import Hurtle.Types

-- Display used for gloss (title, window size, window position/offset)
windowDisplay :: Display
windowDisplay = InWindow "Hurtle" (600, 600) (10, 10)

{-
    May or may not load, so it's wrapped inside of Maybe. 
    We pattern match against this later in case it does not load. 
    It's also lifted into the IO Monad  
-}
turtle :: IO (Maybe Picture)
turtle = loadJuicy "assets/kazki.png"

{-
    display takes in a Simulation object (representing the state after interpreting a hogo program)
    It also takes in a float to represent the speed of the animation. This speed argument is typically
    the speedOfAnimation constant in Main.hs. 

    As mentioned in Main.hs, the result from an IO computation is useless hence the unit () for the result.
    This is because we only care about the state from the IO monad. 

    We get the turtle value wrapped inside IO to get ourTurtle :: Maybe Picture.
    We use a case statement to pattern match on the turtle to check whether the image loaded properly or not.
    If it did not load, then we output to the user that it failed to load.
    If it did load, then we continue to the animation. 

    We use let statements to make the code easier to read. Same reason for using do instead of a sequence of binds.
-}
display :: Simulation -> Float -> IO ()
display simulation speed = do 
    ourTurtle <- turtle 

    -- turtle could be Nothing! 
    case ourTurtle of 
        -- image loaded 
        (Just turtleImage) -> do 
            let pathPoints = path simulation 
            let scaledTurtleImage = Scale 0.2 0.2 turtleImage
            animate windowDisplay white $ runAnimation simulation speed scaledTurtleImage $ getTotalTimeOfAnimation pathPoints speed
        -- image did not load
        Nothing -> putStrLn "The turtle failed to load!"
    
{-
    getTotalTimeOfAnimation takes in a list of path points, and the speed of the animation. 
    It calculates the time taken to traverse the path, considering the speed of the animation.

    A case for no points is established so that we can safely use head in the main case (where number of points >= 1).

    A where binding is used where the helper function addToTotalTime is declared. 
    This helper function takes in the first point and the next point since we are considering the time 
    to traverse between these two points. t here represents the current time taken to execute. 
    The helper function increments t and then returns the new value of t. 
    Either t is increased (when there is a distance travelled), or if the traversal is instant then t remains the same.

    We use the custom foldLPoints function to calculate the total time to execute an animation. 
    We pass in the initial time to execute (0 seconds), the initial first point (hence head), 
    and the list of points that we calculate the distance between then time to travel between.
-}
getTotalTimeOfAnimation :: [PathPoint] -> Float -> Float 
getTotalTimeOfAnimation [] _ = 0 -- no points means no time spent (although I don't think this is possible?)
getTotalTimeOfAnimation points speed = foldLPoints addToTotalTime 0 (head points) (tail points) 
-- head is safe, in the above case, to use since case of no points is dealt with first
    where 
          addToTotalTime :: PathPoint -> PathPoint -> Float -> Float 
          addToTotalTime pointA pointB t
            | instantaneous pointB = t
            | otherwise = t + getTimeForLine speed pointA pointB

{-
    Foldl does not allow you to easily have a transformation function that depends on two variables.
    Perhaps this could have been achieved using a pair however it would be very contrived. 
    The complexity of such a function justifies the creation of foldLPoints. 

    foldLPoints takes in a function that, given the a point and the next point along, as well as some sort of 
    "counter" variable returns an incremented version of the "counter" variable.
    In our case, we call foldLPoints with time as a float. 

    It also takes in the initial state of the counter variable.
    It also takes the first point to consider and the list of next points. 
    When foldL folds through the list, the first point is swapped for the current next point 
    in the next iteration. This happens until the last point is reached which does not have a next point after.
    foldL returns the final state of the counter variable after folding through the entire list of next points. 

    Its simplest case is when the list of next points is empty. 
    There is nothing to fold over in this case, so the initial counter variable state is returned. 

    The next simplest case is also the base case for the complex recursive case.
    If there is a single next point remaining, then we transform the counter variable through the function. 
    In our case, the time is incremented by the time taken to traverse between the initial point and the
    only point left in the list. 

    The recursive case uses pattern matching to split the points into a point to consider and the rest of them.
    We then calculate the new time to traverse for the next recursion.
    We calculate it as the current time elapsed plus the time taken to traverse between this point and the next
    inside the list (the first point in the nextPoints list which is exposed by the pattern matching).
    We also recurse with the nextpoints as the current nextpoints minus the first point in the list. 
-}
foldLPoints :: (PathPoint -> PathPoint -> a -> a) -> a -> PathPoint -> [PathPoint] -> a
foldLPoints _ initialTime _ [] = initialTime                                                 
foldLPoints f initialTime initialPoint [point] = f initialPoint point initialTime -- base case for the recursive case (+ single case)    
foldLPoints f initialTime initialPoint (point:otherPoints) = foldLPoints f (f initialPoint point initialTime) point otherPoints

{-
    runAnimation uses a guard statement to determine whether to run the primary or secondary animation

    Unfortunately, there was not enough time to implement the secondary animation
    The idea was that if the animation time was beyond the time to draw the animation
    then we could move onto the secondary animation which would draw the final state and mutate it
    with a secondary animation 
-}
runAnimation :: Simulation -> Float -> Picture -> Float  -> Float -> Picture 
runAnimation simulation speed turtleImage tt t 
    | t > tt = runPrimaryAnimation simulation speed t turtleImage -- secondary animation
    | otherwise = runPrimaryAnimation simulation speed t turtleImage

{-
    runPrimaryAnimation obtains the line images, and turtle state in order to 
    draw the images to the gloss window 

    We know that the Picture is opted into the Semigroup typeclass so we can use <> 
    which has a notion of combining 
-}
runPrimaryAnimation :: Simulation -> Float -> Float -> Picture -> Picture 
runPrimaryAnimation simulation speed t turtleImage = let 
       (pathLines, turtlePosition, turtleAngle) = primaryAnimationLines (reverse $ path simulation) speed t []
    in 
       drawTurtle turtlePosition turtleAngle turtleImage <> superimposeLines pathLines

{-
    Draw turtle draws the turtle using gloss' transformation methods
    Note that the rotation angle may seem odd but this is because gloss treats the angle as
    the clockwise angle whereas we store the counterclockwise angle (because that's what sin/cos work with)
    Therefore, we inverse the angle (* -1) and then we shift it by a constant 90 degrees to accomodate our image. 

    We use pattern matching to break down the Point (which is a type alias for (Float, Float)) into its components x, y. 
    This is so we can pass it to translate. 
    We also use the low precedence function application $ in order to avoid unnecessary brackets. 
-}
drawTurtle :: Point -> Float -> Picture -> Picture 
drawTurtle (x,y) turtleAngle turtleImage = translate x y $ rotate (-turtleAngle + 90) turtleImage 

{-
    superimposeLines is a recursive function with a base case (also the same as the zero case), 
    and a recursive case (for number of lines >= 1).
    We use pattern matching to break down the recursive case into a line prepended in front of a list of other lines. 
    The list of other lines may be empty (in which case the next recursive step leads into the base case!)

    We use the binary associative operator that the Semigroup typeclass provides us (<>) since we need 
    something with a notion of combining and Picture opts into the Semigroup typeclass. 
-}
superimposeLines :: [Picture] -> Picture 
superimposeLines [] = Blank 
superimposeLines (aLine:otherLines) = aLine <> superimposeLines otherLines 

{-
    primaryAnimationLines generates the lines and also the state of the turtle by recursively
    iterating through the points in the lines 
    
    The simplest case is of no points which I don't think is actually possible, but it would return
    no lines and the default state of the turtle.

    The next simplest case is also the base case for the recursive case. 
    It occurs when there is only one point left. 
    If it is a clearing point (meaning a GoHome or ClearScreen action) then it will reset all lines. 
    If not, then it simply returns the current lines drawn.
    In both cases, it returns the state of the turtle at that last point. 

    The recursive case uses pattern matching to break down the points. 
    The rest of the points, represented by _, can be an empty list here. 
    The recursive case adds no lines in the case of it being instantaneous (summoning back home 
    via GoHome or Clearscreen).

    We either draw a line fully and recurse to the next lines to draw.
    Or, the time in the animation is such that the line is not fully drawn.
    In this case, we draw the line partially and do not recurse.
    We draw the line partially using linear interpolation:
    that is, we draw the line between the first point and part of the way to the second point. 
-}
primaryAnimationLines :: [PathPoint] -> Float -> Float -> [Picture] -> ([Picture], Point, Float)
primaryAnimationLines [] _ _ _ = ([Blank], (0, 0), 90) -- no points -> no lines
primaryAnimationLines pathPoints speed t currentLines = case pathPoints of {
    [x] -> case clearing x of { 
        True -> ([], position x, pathTurtleAngle x); -- last point is a clearing point 
        False -> (currentLines, position x, pathTurtleAngle x) -- we've drawn all the points! 
    };
    -- if either it's an instantaneous move or if the point is registered with an inactive pen, then don't draw
    -- still update the position if it's an inactive pen though 
    (pointA:pointB:_) -> case instantaneous pointB of {
                            True -> primaryAnimationLines (tail pathPoints) speed t currentLines; -- we do not draw a line; we skip to the next recursion
                            False ->
                            let 
                                timeToDraw :: Float 
                                timeToDraw = getTimeForLine speed pointA pointB 

                                linesToDraw :: [Picture]
                                -- if pointA is clearing, then all lines before and connected to pointA be cleared
                                linesToDraw = if clearing pointA then [] else currentLines
                            in   
                                -- let's see if we can fully draw the line, or whether we have to lerp it 
                                case timeToDraw >= t of {
                                    True -> let 
                                                timeDelta :: Float 
                                                timeDelta = t/timeToDraw 
                                                
                                                newPointB :: PathPoint 
                                                newPointB = lerp timeDelta pointA pointB 
                                            in 
                                                if active $ pathPenState pointB then 
                                                    -- active pen so we draw the line 
                                                    (drawLine pointA newPointB : linesToDraw, position newPointB, pathTurtleAngle newPointB); -- timeToDraw is >= time, so we have to lerp 
                                                else
                                                    -- inactive pen, so we just update position 
                                                    (linesToDraw, position newPointB, pathTurtleAngle newPointB);
                                    False -> if active $ pathPenState pointB then -- we can draw the entire line; we have to recurse 
                                                -- active pen so we draw the line  
                                                primaryAnimationLines (tail pathPoints) speed (t - timeToDraw) $ drawLine pointA pointB : linesToDraw 
                                             else 
                                                -- inactive pen, so we just update position 
                                                primaryAnimationLines (tail pathPoints) speed (t - timeToDraw) $ linesToDraw
                                }
                          }               
}

{-
    Lerp function is for linearly interpolating between two points
    Returns the point which is t of the way into the path between pointA and pointB 

    Uses a let binding for cleaner computation 
    Uses accessor functions to access positions 

    Record syntax is used for easy creation of a new PathPoint object based off PointB
-}
lerp :: Float -> PathPoint -> PathPoint -> PathPoint 
lerp t pointA pointB = let 
                           lerpFloat :: Float -> Float -> Float -> Float 
                           lerpFloat dt a b = a + dt * (b-a)

                           posA :: Point 
                           posA = position pointA  

                           posB :: Point 
                           posB = position pointB 

                           -- the direction is from a to b
                           inbetweenPoint :: Point 
                           inbetweenPoint = (lerpFloat t (fst posA) $ fst posB, lerpFloat t (snd posA) $ snd posB)
                       in 
                           -- new point B 
                           pointB {
                                position = inbetweenPoint
                           }

{-
    drawLine returns a picture representing a line drawn between two points.
    It uses the line primitive inside of gloss and uses accessor function position to pass position to it. 
    Then it applies accessor functions to obtain the gloss colour to modify the line. 
-}
drawLine :: PathPoint -> PathPoint -> Picture 
drawLine pointA pointB = color (unwrapColor $ colour $ pathPenState pointB) $ line [position pointA, position pointB] 
          
{-
    getTimeForLine calculates the time taken to traverse between two points at a given speed

    A where binding is used to simplify the computation 
    difference is the difference vector to represent how to get from pointB to pointA 
    Pythagoras' theorem is used to calculate the magnitude of this difference which is the distance
    between the points pointA and pointB 

    Physics A-Level moment is applied to figure out that time = distance/speed 
    Maybe physics was my true calling 
-}
getTimeForLine :: Float -> PathPoint -> PathPoint -> Float 
getTimeForLine speed pointA pointB = distance/speed
    where
          posA :: Point 
          posA = position pointA 

          posB :: Point 
          posB = position pointB 

          difference :: Point 
          difference = (fst posA - fst posB, snd posA - snd posB)

          distance :: Float 
          distance = sqrt $ (fst difference)**2 + (snd difference)**2