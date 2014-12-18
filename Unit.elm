module Unit where

{-| A Unit testing Library for Elm 

# Types

@docs Test, TestGroup, Suite

# Operators

@docs (:-)

# Samples

@docs sampleSuite, arithmeticPass, arithmeticFail, booleanPass, booleanFail

# Invoking

@docs digest, run, runWith, runTests, runTestsWith, displaySuite

# Utilities

@docs mergeSuites

# Comparators

@docs fequal, fequalr

-}

import List (..)
import Text (..)
import Graphics.Element (..)
import Util.Function (..)

join : String -> List String -> String
join s l = case l of
  [] -> ""
  (x::[]) -> x
  (x::xs) -> x ++ s ++ join s xs

{-| A single test made up of a name, a function, and a list of inputs and their expected outputs -}
type alias Test a b = (String, (a -> b), List (a, b))

{-| A test group made up of the name of the test group, and a list of tests in the group -}
type alias TestGroup = (String, List String)

{-| A suite made up of multiple String digests -}
type alias Suite = List TestGroup

{-| An alias for tuppling for use in test formatting -}
(:-) = (,)

{- Example test suites -}

{-| An example test suite containing tests of basic arithmetic and boolean operators -}
sampleSuite : Suite
sampleSuite = 
  [ ("Arithmetic Tests (Success Example)",
    [ run ("(+)", uncurry (+),
        [ (1, 2) :- 3
        , (2, 3) :- 5
        , (5, -3) :- 2 ] )
    , run ("(-)", uncurry (-),
        [ (2, 1) :- 1
        , (8, 4) :- 4
        , (2, -3) :- 5 ]) 
    , run ("(*)", uncurry (*),
        [ (5, -1) :- -5
        , (1, 2) :- 2
        , (7, 3) :- 21 ])
    , run ("(/)", uncurry (/),
        [ (1, 2) :- 0.5
        , (9, 3) :- 3
        , (-4, 2) :- -2 ]) ])
  , ("Arithmetic Tests (Failure Example)",
    [ run ("(+)", uncurry (+),
        [ (1, 2) :- 7
        , (2, 3) :- 1.5
        , (5, -3) :- 3 ] )
    , run ("(-)", uncurry (-),
        [ (2, 1) :- 2
        , (8, 4) :- -4
        , (2, -3) :- 6 ]) 
    , run ("(*)", uncurry (*),
        [ (5, -1) :- -4
        , (1, 2) :- 2.1
        , (7, 3) :- 22 ])
    , run ("(/)", uncurry (/),
        [ (1, 2) :- 0
        , (9, 3) :- -1
        , (-4, 2) :- 10 ]) ]) 
  , ("Boolean Tests (Success Example)",
    [ run ("(==)", uncurry (==),
        [ (True, True) :- True
        , (True, False) :- False
        , (False, False) :- True ])
    , run ("(/=)", uncurry (/=),
        [ (True, True) :- False
        , (True, False) :- True
        , (False, False) :- False ])
    , run ("(&&)", uncurry (&&),
        [ (True, True) :- True
        , (True, False) :- False
        , (False, False) :- False ])
    , run ("(||)", uncurry (||),
        [ (True, True) :- True
        , (True, False) :- True
        , (False, False) :- False ])])
  , ("Boolean Tests (Failure Example)",
    [ run ("(==)", uncurry (==),
        [ (True, True) :- False
        , (True, False) :- True
        , (False, False) :- False ])
    , run ("(/=)", uncurry (/=),
        [ (True, True) :- True
        , (True, False) :- False
        , (False, False) :- True ])
    , run ("(&&)", uncurry (&&),
        [ (True, True) :- False
        , (True, False) :- True
        , (False, False) :- True ])
    , run ("(||)", uncurry (||),
        [ (True, True) :- False
        , (True, False) :- False
        , (False, False) :- True ])
    ])]


digest : (b -> b -> Bool) -> (a -> b) -> (String, (a, b)) -> String
digest compare f (name, (args, expected))
  = let pass = compare (f args) expected
        out = f args
        result = if pass then "pass" else "FAIL"
    in "\"" ++ name ++ "\"" ++ " of " ++ (toString args) ++ " => "
    ++ (toString out) ++ ", expected " ++ (toString expected) 
    ++ " (" ++ result ++ ")"

{-| Runs a test with (==) as the comparator -}
run : Test a b -> String
run = runWith (==)

runWith : (b -> b -> Bool) -> Test a b -> String
runWith compare (name, f, cases)
  = let names = repeat (length cases) name
        zipped = map2 (,) names cases
    in join "\n" (map (digest compare f) zipped)

{-| Merges a list of test suites into a single test suite -}
mergeSuites : List Suite -> Suite
mergeSuites suites = foldr (++) [] suites

formatTestGroup : TestGroup -> String
formatTestGroup (name, results)
  = "Test Group: " ++ name ++ "\n\n" ++ (join "\n" results) ++ "\n"

{-| Gets the results of a test suite as an Graphics.Element.Element for displaying -}
displaySuite : String -> Suite -> Element
displaySuite name results
  = leftAligned ((bold (fromString name)) ++
    (fromString "\n\n") ++
    (fromString (join "\n" (map formatTestGroup results))) ++
    (fromString "\n"))

{-| Checks if floating point numbers are nearly equal. This is equivalent to fequalr with an epsilon of 0.000001 -}
fequal : Float -> Float -> Bool
fequal = fequalr 0.000001

{-| Checks if floating point numbers are equal, or very close to equal -}
fequalr : Float -> Float -> Float -> Bool
fequalr epsilon a b
  = (abs (a - b)) < epsilon




