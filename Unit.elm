module Unit where

import Util.Function (..)

type Test a b = (String, (a -> b), [a], [b])

type TestGroup a b = (String, [Test a b])

type Suite = [String]

{- Example test suites -}

sampleSuite : Suite
sampleSuite = [arithmeticPass, arithmeticFail, booleanPass, booleanFail]

arithmeticPass : String
arithmeticPass = runTests ("Arithmetic (pass example)",
  [ ("(+)", uncurry (+), [(1, 2), (2, 3), (5, -3)], [3, 5, 2])
  , ("(-)", uncurry (-), [(2, 1), (8, 4), (2, -3)], [1, 4, 5]) 
  , ("(*)", uncurry (*), [(5, -1), (1, 2), (7, 3)], [-5, 2, 21])
  , ("(/)", uncurry (/), [(1, 2), (9, 3), (-4, 2)], [0.5, 3, -2]) ] )

arithmeticFail : String
arithmeticFail = runTests ("Arithmetic (fail example)",
  [ ("(+)", uncurry (+), [(1, 2), (2, 3), (5, -3)], [6, -2, 4])
  , ("(-)", uncurry (-), [(2, 1), (8, 4), (2, -3)], [8, 3, 1]) 
  , ("(*)", uncurry (*), [(5, -1), (1, 2), (7, 3)], [-2, 3, 21.3])
  , ("(/)", uncurry (/), [(1, 2), (9, 3), (-4, 2)], [0.1, 320, -25]) ] )

booleanPass : String
booleanPass = runTests ("Boolean Operators (pass example)",
  [ ("(==)", uncurry (==), [(True, True), (True, False), (False, False)], 
      [True, False, True])
  , ("(/=)", uncurry (/=), [(True, True), (True, False), (False, False)], 
      [False, True, False])
  , ("(&&)", uncurry (&&), [(True, True), (True, False), (False, False)],
      [True, False, False])
  , ("(||)", uncurry (||), [(True, True), (True, False), (False, False)],
      [True, True, False]) ])

booleanFail : String
booleanFail = runTests ("Boolean Operators (fail example)",
  [ ("(==)", uncurry (==), [(True, True), (True, False), (False, False)], 
      [False, True, False])
  , ("(/=)", uncurry (/=), [(True, True), (True, False), (False, False)], 
      [True, False, True])
  , ("(&&)", uncurry (&&), [(True, True), (True, False), (False, False)],
      [False, True, True])
  , ("(||)", uncurry (||), [(True, True), (True, False), (False, False)],
      [False, False, True]) ])

digest : String -> a -> b -> b -> String
digest name args out expected
  = let result = if out == expected then "pass" else "FAIL" 
    in "\"" ++ name ++ "\"" ++ " of " ++ (show args) ++ " => " ++ (show out) ++
       ", expected " ++ (show expected) ++ " (" ++ result ++ ")\n"

run : Test a b -> String
run (name, f, argsList, expectedList)
  = let outList = map f argsList
        names = (repeat (length argsList) name)
        testCases = zip4 names argsList outList expectedList
        results = map (uncurry4 digest) testCases
    in "\"" ++ name ++ "\"" ++ " tests:\n\n" ++ (join "" results)

runTests : TestGroup a b -> String
runTests (name, tests)
  = let suiteResults = map run tests 
    in "Group: " ++ name ++ "\n\n" ++ (join "\n" suiteResults)

displaySuite : String -> Suite -> Element
displaySuite name results
  = leftAligned ((bold (toText name)) ++
    (toText "\n\n") ++
    (toText (join "\n" results)) ++
    (toText "\n"))

