-- This is the main entry point for your Hurtle viewer.
import Control.Monad (unless)
import Control.Monad.State
import System.IO
import Text.Megaparsec

-- Our Modules
import Hurtle.Parser
import Hurtle.SimulationState 
import Hurtle.Viewer 

{-
    A constant for how fast the animation should run.
    Even though 70.0 looks big, it's just because there's a lot of distance to cover per second.
    I would not recommend decreasing this. 
-}
speedOfAnimation :: Float 
speedOfAnimation = 70.0 

{-
    readProgram takes in a filePath and returns an IO action which makes calls to parse, run, and display the program.
    Since we don't care about the result of IO, its type is IO () since the unit represents nothing useful. 
    We only care about its state. 

    We use do notation to sequence together IO actions because it would be cumbersome to use 
    and really confusing to use bind (>>=) to sequence them together. 

    readFile is used on the filePath passed and if it doesn't exist, then Haskell will throw an IO exception for us.
    If it does exist, then we pattern match on what the parse function returns via a case statement.
    We could've used a let binding to make it clearer that parse parseHogo "" contents is the parsedProgram,
    however I thought it was readable enough as it is. 

    If the parse function is unsuccessful, then we report the error.
    Else, we continue on and run the stateful computation inside of SimulationState.hs on the parsed contents. 

    After getting the state from executing the stateful computation (similarly, the result of the stateful computation
    does not matter so it is () and we discard it) by using snd to access the state only, we run the viewer in 
    Viewer.hs with the final state. 

    Once display stops keeping all of the running for itself, we call main. 
    Equivalently, if there is a parsing error then we call main.
    Therefore, we recurse back to the bit where it says "Enter path ..."
    This is a proper tail recursion so it doesn't consume any unnecessary memory from having to store 
    stack frames.  
-}
readProgram :: String -> IO ()
readProgram filePath = do 
    contents <- readFile filePath -- Haskell gives us a nice error message if this filePath is invalid 

    case parse parseHogo "" contents of {
        Left errBundle -> putStrLn $ errorBundlePretty errBundle;
        Right parsedProgram -> do 
            let simulation = snd $ runState (runSimulation parsedProgram) initialState 
            display simulation speedOfAnimation
    }

    -- go back to main, to allow user to run more programs
    main 

{-
    main represents an IO action. It receives input from the user, and either quits (if requested) 
    or calls the readProgram function which will attempt to read in a file and run the contents of the file
    as a HogoProgram.

    As mentioned above, the IO monad is of type IO () because we do not care about its return value so 
    its return type is unit. We only care about its state. 

    We use putStr which is an IO action, followed by using hFlush to flush the output buffer.
    This is because putStr, unlike putStrLn, does not automatically flush the buffer to display the characters
    in the console. It's like not putting endl (which flushes the output buffer) in your C++ code. 

    After the text is displayed to the screen, the user can enter a string which will be stored in 
    filePath :: String. getLine is an IO action and filePath is the unlifted version of what it returns. 

    We then use the unless combinator from Control.Monad to check if the user is trying to quit. 
    If they are not, then we continue to read. If they are, then computation stops. 

    The do here is unnecessary however it makes it easier to read which is why it's there. 
-}
main :: IO ()
main = do 
    putStr "Enter path to a HogoCode file (or q/quit): "
    hFlush stdout 

    filePath <- getLine 

    unless (filePath == "q" || filePath == "quit") $ do -- give user an option to quit 
        readProgram filePath