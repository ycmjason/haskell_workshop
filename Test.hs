module Test where
import Challenges01
import Challenges02
import Challenges03
import System.Environment
-- challenges01
with args _ expected = (args, expected);
expectToBe = "this is useless, just for readability"
isFifteenExpectations = [with 0 expectToBe 'F',
                         with 15 expectToBe 'T',
                         with (-15) expectToBe 'F']

biggerExpectations = [with (1, 1) expectToBe 1,
                      with (10, 2) expectToBe 10,
                      with (-100, 0) expectToBe 0]

biggestOf3Expectations = [with (1, 2, 3) expectToBe 3,
                          with (0, 0, 0) expectToBe 0,
                          with (-100, 100, 50) expectToBe 100,
                          with (65, 34, 22) expectToBe 65]
-- challenges02
lenExpectations = [with [] expectToBe 0,
                   with [1, 2, 3, 4] expectToBe 4,
                   with [1, 23] expectToBe 2,
                   with [1,1,3] expectToBe 3,
                   with [0] expectToBe 1,
                   with [1..3] expectToBe 3]
containsExpectations = [with (0, [300,0,7]) expectToBe True,
                        with (0, []) expectToBe False]
divisibleByExpectations = [with (5, 3) expectToBe False,
                           with (4, 2) expectToBe True,
                           with (3, 7) expectToBe False,
                           with (0, 1) expectToBe True,
                           with (10, 100) expectToBe False]
isPrimeExpectations = [with 2 expectToBe True,
                       with 3 expectToBe True,
                       with 2053 expectToBe True,
                       with 4 expectToBe False,
                       with 101 expectToBe True,
                       with 15 expectToBe False,
                       with 31 expectToBe True,
                       with 53 expectToBe True,
                       with 59 expectToBe True,
                       with 57 expectToBe False]
nextPrimeExpectations = [with 2 expectToBe 3,
                         with 0 expectToBe 2,
                         with 3 expectToBe 5,
                         with 4 expectToBe 5,
                         with 2053 expectToBe 2063,
                         with (-1) expectToBe 2]
biggestExpectations = [with [1] expectToBe 1,
                       with [1..100] expectToBe 100,
                       with [7, 8, 0, 9, 4, 3, 1, 6, 5, 2] expectToBe 9,
                       with [-100,0] expectToBe 0]
-- challenges03
generatePrimesExpectations = [with 10 expectToBe [2,3,5,7],
                              with 100 expectToBe [2,3,5,7,11,
                                                   13,17,19,23,
                                                   29,31,37,41,
                                                   43,47,53,59,
                                                   61,67,71,73,
                                                   79,83,89,97],
                              with 0 expectToBe [],
                              with 1 expectToBe [],
                              with 2 expectToBe [2]]
isSubStringOfExpectations = [with ("I am lovely", "I am lovely") expectToBe True,
                             with ("yo", "yyso") expectToBe False,
                             with ("  ", "I am a handsome Guy!") expectToBe False,
                             with ("\'", "\'This is it\'") expectToBe True]
findExpectations = [with ("Jason", ["Keith", "Tom"]) expectToBe [],
                    with ("apple", ["apple chan", "appltepom ng", "apple"]) expectToBe ["apple chan", "apple"],
                    with ("a", ["a b c", ""]) expectToBe ["a b c"],
                    with ("", ["123","whatsoever"]) expectToBe ["123","whatsoever"]]
findAllExpectations = [with ("Jason", [["noJas.on", "there is no jason"], ["only peter is here"]]) expectToBe [],
                       with ("Jason", [["Jason1", "Jaso n not", "Jason 2"],["Jason 3", "Apple"]]) expectToBe ["Jason1", "Jason 2", "Jason 3"],
                       with ("", [[],[],[],[]]) expectToBe [],
                       with ("", [["abc"],[],[],[]]) expectToBe ["abc"]]

-- expectations for challenges
testExpectation fname f expectations = uncurry (expect fname f) expectations
  where
    expect fname f args expected
      | expected == actual = ""
      | otherwise          = noMatchMsg
      where
        actual = f args
        noMatchMsg = "Failed test case, " ++ fname ++ " " ++ show args ++ ":\n" ++
                     "  Expected: " ++ show expected ++ "\n  Actual: " ++ show actual ++ "\n"


testExpectations fname f expectations = concat (map (testExpectation fname f) expectations)

-- you don't need to change things below
-- tests
challengesTests = map tests [0..2]

tests 0 = [isFifteenTest, biggerTest, biggestOf3Test]
  where
    isFifteenTest = testExpectations "isFifteen" isFifteen isFifteenExpectations
    biggerTest = testExpectations "bigger" (uncurry bigger) biggerExpectations
    biggestOf3Test = testExpectations "biggestOf3" (uncurry3 biggestOf3) biggestOf3Expectations

tests 1 = [lenTest, containsTest, divisibleByTest, isPrimeTest, nextPrimeTest, biggestTest]
  where
    lenTest = testExpectations "len" len lenExpectations
    containsTest = testExpectations "contains" (uncurry contains) containsExpectations
    divisibleByTest = testExpectations "divisibleBy" (uncurry divisibleBy) divisibleByExpectations
    isPrimeTest = testExpectations "isPrime" isPrime isPrimeExpectations
    nextPrimeTest = testExpectations "nextPrime" nextPrime nextPrimeExpectations
    biggestTest = testExpectations "biggest" biggest biggestExpectations

tests 2 = [generatePrimesTest, isSubStringOfTest, findTest, findAllTest]
  where
    generatePrimesTest = testExpectations "generatePrimes" generatePrimes generatePrimesExpectations
    isSubStringOfTest = testExpectations "isSubStringOf" (uncurry isSubStringOf) isSubStringOfExpectations
    findTest = testExpectations "find" (uncurry find) findExpectations
    findAllTest = testExpectations "findAll" (uncurry findAll) findAllExpectations
-- uncurry
uncurry3 f (x, y, z) = f x y z

-- MAIN
main = do  
  args <-  getArgs
  -- args !! 0 == challengeNumber
  if length args > 0 then
    putStr $ runTest (read (args !! 0)::Int)
  else
    putStr $ runAllTests
  putStrLn "Test run complete. All tests passed if you don't see any errors."

runTest n = concat $ challengesTests!!(n-1)
runAllTests = concat $ map (concat) challengesTests
