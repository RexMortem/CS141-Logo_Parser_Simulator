module Hurtle.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (when)
import Graphics.Gloss -- for the Color datatype
import qualified Text.Megaparsec.Char.Lexer as Lexer -- we import qualified due to naming collision e.g. space 

-- Our Modules
import Hurtle.Types

--[[ Entry Point 

{-
    parseHogo uses the between combinator to apply a parser to the content 
    which is inbetween two other parsers.

    The consumeEmptySpace is the opening parser which consumes space and comments ("empty space") until it can't. 
    eof is the end of file which means that the inbetween is the entirety of the useful program. 
    This inbetween is then parsed as many times as it can be using parseAction, which parses a HogoCode 
    from the program (HogoCode represents an action/statement in the language).  

    We apply consumeEmptySpace at the start because parseAction handles consuming the space after parsing
    each HogoCode action but only AFTER parsing one. So we need to deal with the case where there is 
    "empty space" (space and comments) before the first statement. 
-}
parseHogo :: Parser HogoProgram
parseHogo = between consumeEmptySpace eof $ many parseAction

{-
    parseAction uses the choice combinator to try each of the parsers in turn. 
    If one fails, then the choice combinator tries the next one. 
    If they all fail, then we have an unparsable part of the program -> syntax error. 

    We also use map to wrap each of these parsers to try. This wrapping makes them 
    consume "empty space" (comments and space) after parsing if they successfully parse. 
    This of course works because [] is a monad so is a functor so is mappable. 

    This acts as a control parser for calling the main parsers of the language. 
-}
parseAction :: Parser HogoCode 
parseAction = choice $ map wrapCommand [
                    -- no argument actions 
                      parsePenUp
                    , parsePenDown
                    , parseGoHome
                    , parseClearScreen
                    
                    -- standard actions
                    , parseMovementAndRotation

                    -- complex/block actions 
                    , parseRepeat

                    -- extension actions 
                    , parseColour
                    ]


--[[ Constants 

{-
    This is unused.
    This was going to be used for applying rules to subroutine naming.
    Subroutines were not implemented.  
-}
hogoKeywords :: [String]
hogoKeywords = [""]

--[[ Helpers 

{-
    wrapCommand wraps a parser and returns a parser with an extra bit of functionality:
    it consumes "empty space" (space and comments) after the parser successfully parses an action. 
    For ease of use and reusability, we use the Lexer library 
-}
wrapCommand :: Parser a -> Parser a 
wrapCommand = Lexer.lexeme consumeEmptySpace

{-
    Consumes all "empty" space: either space, newlines/carriage-returns, or a comment (or both)
    For reusability, we use the Lexer library 
    We pass empty for the block comment argument as Hogo does not have block comments 
-}
consumeEmptySpace :: Parser ()
consumeEmptySpace = Lexer.space space1 consumeLineComment empty 
    where 
          consumeLineComment = Lexer.skipLineComment ";"

{-
    Use hSpace here because we don't want comments or newlines between action and arguments
    We do allow normal space between arguments in actions though.
    For example, forward 5.5 and forward    5.6 is allowed. 
-}
consumeActionSpace :: Parser ()
consumeActionSpace = hspace 

{-
    Parse number takes in a number of digits to form the integer part of the number.
    Some is used so that at least one digit is required. If no digits are supplied,
    then the number is invalid. This is why forward with no number should fail.

    intPart is the unlifted sequence of characters (a string).

    The decimal part is an optional part of the number so the option combinator is used. 
    If there is no decimal part, then the empty string is appended.
    So the number 5 is as valid as the number 5.62 for parsing to a float. 

    If the decimal part exists, then it is unlifted and appended to the intPart and then lifted via pure.
    Before it is lifted, read is used to convert the string to a float. 
    This should not fail and Haskell knows the type to be a float from the type declaration of parseNumber.

    parseNumber uses a where binding to declare the helper function getDecimalPart.
    getDecimalPart takes in the intPart as an argument purely for an enhanced failure message. 

    While we use many here, we do not actually respect an input such as 5.
    This is dealt with in the if statement which checks if there are any digits using null
    If there are digits, then this is lifted with the dot appended to digits so it can be appended 
    to the int part to be converted to a float. 
-}
parseNumber :: Parser Float 
parseNumber = do
    intPart <- some digitChar -- part before the decimal point (or the entire number if it's an integer)
    decimalPart <- option "" $ getDecimalPart intPart
    pure $ read $ intPart ++ decimalPart
    where 
          getDecimalPart :: String -> Parser String 
          getDecimalPart intPart = do
            char '.' -- consumed input at this point; any failures past this point will fail all of parseNumber
            digits <- many digitChar  

            if null digits then 
                fail("Invalid digits for number beginning with " ++ intPart ++ "!") 
            else
                pure $ '.' : digits 

-- only returns False for x.y where y = 0 
cantConvertToInt :: Float -> Bool 
cantConvertToInt n = floor n /= ceiling n 

--[[ No argument actions 

{-
    These four parsers are simple enough that a do is omitted. 
    They simply check if a certain input exists, and then lifts a constructor via pure. 
-}
parsePenUp :: Parser HogoCode 
parsePenUp = string "penup" >> pure PenUp 

parsePenDown :: Parser HogoCode 
parsePenDown = string "pendown" >> pure PenDown 

parseGoHome :: Parser HogoCode 
parseGoHome = string "home" >> pure GoHome 

parseClearScreen :: Parser HogoCode 
parseClearScreen = string "clearscreen" >> pure ClearScreen

--[[ Single argument actions 

{-
    For reusability of code, we lump in movement and rotation since they both parse an action plus a float argument 

    parsedAction is an unlifted Float -> HogoCode function. Choice is used because there are a significant number of parsers
    that we want to try to check for possible movement and rotation actions. 

    <$> can be used here because the Parser is a monad and therefore also a functor. 
    It is used as parsedAction is an unlifted function, however the number returned
    from parseNumber is lifted (wrapped inside the Parser).
    So we need to use <$> to apply the parsedAction function to it. 
    The end result is the lifted and fully formed HogoCode. 

    <?> here is a combinator used to label the parsing for additional error information 
    
-}
parseMovementAndRotation :: Parser HogoCode
parseMovementAndRotation = do 
    -- check against each possible action
    parsedAction <- choice [parseForward, parseBackward, parseTurnLeft, parseTurnRight]
    consumeActionSpace
    parsedAction <$> parseNumber <?> "Valid number"
    where 
          -- movement 
          parseForward = string "forward" >> pure GoForward 
          parseBackward = string "back" >> pure GoBackward 

          -- rotation 
          parseTurnLeft = string "left" >> pure TurnLeft
          parseTurnRight = string "right" >> pure TurnRight

--[[ Block actions 

{-
    parseRepeat confirms that the action is "repeat" else it fails at the start

    It then checks if a number is provided. We re-use parseNumber however this number is a float. 
    We unlift it into nTimes :: Float.

    nTimes may be convertible into an integer if it is of the form x.0
    We check this using the function cantConvertToInt :: Float -> Bool 
    which is defined above 

    We use the when combinator to fail the parser when the expected number of times is not an integer
    We also use the when combinator to fail when the expected number is negative 
    or 0 because repeating 0 times is useless. 

    When the number is valid, we consume empty space beacuse we allow comments and empty space 
    between the number and the actual repeat block of code

    We need to confirm the actual block of code contains valid instructions, so we run
    the main parser for all the actions within the repeat block (many parseAction).
    We do this between the square brackets which define the block of HogoCode inside the repeat block.

    We use the between combinator for this and we have to use <* consumeEmptySpace 
    to allow for newlines and comments after the first square bracket and before the first bit of the 
    repeat block of code. 
    
    Remember that, for the main program, consuming the first bit of empty space 
    is dealt with in parseHogo just before many parseAction. 

    We can use <* since the Parser is a monad and therefore an applicative. It consumes space after parsing the '['
    character existing. 

    Finally, the action is encoded using the Repeat HogoCode constructor and lifted via pure. 
-}
parseRepeat :: Parser HogoCode 
parseRepeat = do
    string "repeat"
    consumeActionSpace
    nTimes <- parseNumber 

    -- we must now verify whether nTimes is a valid integer 

    when (cantConvertToInt nTimes) $ fail "Expected integer as the argument for repeat!"

    let nTimesAsInt = round nTimes 

    when (nTimesAsInt <= 0) $ fail "Expected integer greater than 0 for the argument of repeat!"

    -- now that verification for the repeat count is done, let's verify the code itself 

    consumeEmptySpace 
    actions <- between (char '[' <* consumeEmptySpace) (char ']') $ many parseAction -- we allow empty repeats
    consumeEmptySpace 

    pure $ Repeat nTimesAsInt actions 

-- beyond the minimum spec 

{-
    parseColour confirms that the action used is "color" or it fails at the first line.

    parseColour uses the choice combinator to try a series of colour parsers. 
    If one is successfully parsed, then we wrap the colour so that it can be stored inside a datatype
    because the gloss color datatype doesn't implement Show in a normal way. 
    We then turn it into a HogoCode through the Colour constructor and lift it to return it from the parser.

    The colours are from https://hackage.haskell.org/package/gloss-1.0.0.0/docs/Graphics-Gloss-Color.html
-}
parseColour :: Parser HogoCode 
parseColour = do 
    string "color" -- this spelling is from the coursework specification, not preference 
    consumeActionSpace
    chosenColour <- choice [parseBlack, parseWhite, parseRed, parseGreen, parseBlue, parseYellow, parseCyan, parseMagenta] <?> "valid colour"
    pure $ Colour $ WrapColor chosenColour 
    where 
          parseBlack = string "black" >> pure black 
          parseWhite = string "white" >> pure white -- the background is white so this won't show well

          parseRed = string "red" >> pure red
          parseGreen = string "green" >> pure green
          parseBlue = string "blue" >> pure blue

          parseYellow = string "yellow" >> pure yellow
          parseCyan = string "cyan" >> pure cyan 
          parseMagenta = string "magenta" >> pure magenta


{-
    Unfortunately, penWidth had to be disabled because there was not enough time 
    to finish the gloss section of penWidth.
    The parsing and state handling exist for penWidth (commented out), but not the visual implementation.
-}

-- parsePenWidth :: Parser HogoCode 
-- parsePenWidth = do
--     string "penwidth"
--     consumeActionSpace
--     width <- parseNumber 

--     -- we must now verify whether width is a valid integer 

--     when (cantConvertToInt width) $ fail "Expected integer as the argument for penwidth!"

--     let widthAsInt = round width 

--     when (widthAsInt <= 0) $ fail "Expected integer greater than 0 as the argument for penwidth!"

--     pure $ PenWidth widthAsInt  