{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module TestPP 
  ( quickCheck, vmCheck
  , testCond, testVal, testManually, testSet
  , testCondW, testValW, testManuallyW, testSetW
  , test, tests, tests_
  , TestData, TestCase
  ) where

import qualified Control.Exception as E (catch, evaluate, SomeException (..))
import Data.List
import Debug.Trace

{- Defines one test case, internally.
-}
data TestCase = TestCase
  { testIndex   :: Int    -- the number of the excercise
  , testDescr   :: String -- description of the test
  , testPoints  :: Float  -- points for the test case
  , testError   :: String -- error if the test failed
  , testPassed  :: Bool   -- has passed? also lazily contains the actual test
  }

{-Information for testing an entire exercise (is a list of test cases).
-}
newtype TestData = TD {getTestCases :: [TestCase]}

{- Printing a test also checks it -}
instance Show TestData where
  show = concat . intersperse "\n" . map (testMessage . testResult) . getTestCases

showPt :: RealFrac a => a -> String
showPt = show . (* 0.01) . fromIntegral . round . (* 100)

showEx :: Bool -> Int -> Int -> String
showEx showPoints nEx index = "[" ++ (if showPoints then "" else "ex ")
         ++ show index ++ (if showPoints then "" else "/" ++ show nEx) ++ "]"

{- Contains information obtained from running one test case.
-}
data TestCaseResult = TestCaseResult
  { testExercise       :: Int       -- the number of the exercise
  , testMessage        :: String    -- full output of the test
  , testScore          :: Float     -- how much the test actually scored
  , testSubjectUndefined   :: Bool  -- was the exercise undefined?
  , testExcepted       :: Bool      -- has the test resulted in an exception?
  } deriving Show

{- Builds test data for an exercise, based on the descriptions of test cases -}
tests :: Int -> Float -> [TestCase] -> TestData
tests ex points testList = TD $ map tt testList
      where
        total = foldl (+) 0 $ map (\(TestCase _ _ p _ _) -> p) testList
        tt (TestCase _ d p e b) = TestCase ex d (points * p / total) e b

tests_ :: Int -> [TestCase] -> TestData
tests_ ex = tests ex 0

-- use to insert one single test for an exercise
test :: Int -> TestCase -> TestData
test ex (TestCase _ d p e b) = TD [TestCase ex d p e b]

{- Builds the description of a a test case. The value is a boolean.
  The points represent the fraction of the total points of the exercise.
  For the moment the exercise number is unknown
-}
buildTest :: String -> Float -> String -> Bool -> TestCase
buildTest = TestCase 0

{- Describes a test case which evaluates a boolean value. -}
testCondW :: String -> Float -> Bool -> TestCase
testCondW descr points bool = buildTest descr points "boolean test failed" bool

testCond :: String -> Bool -> TestCase
testCond descr = testCondW descr 1


{- Describes a test case which evaluates the equality of two values (expected and given). -}
testValW :: (Show a, Eq a) => String -> Float -> a -> a -> TestCase
testValW descr points ve vg = testWithW descr "value" "equal" points ve (==) vg

testVal descr = testValW descr 1


{- Describes a test case which evaluates the equality of two sets (expected and given). -}
testSetW :: (Show a, Eq a) => String -> Float -> [a] -> [a] -> TestCase
testSetW descr points vg ve = testWithW descr "set" "having same elements" points ve sameElements vg

testSet descr = testSetW descr 1


{- Describes a test case which compares an expected and a given value based on a function (op). vname and opname describe what the values are and what the comparator represents. -}
testWithW :: (Show a1, Show a) => String -> String -> String -> Float
     -> a -> (a -> a1 -> Bool) -> a1 -> TestCase
testWithW descr vname opname points ve op vg = buildTest descr points err (ve `op` vg)
  where err = ("given " ++ vname ++ " " ++ show vg ++ " not " ++ opname ++ " with expected " ++ vname ++ " " ++ show ve)

testWith descr vname opname = testWithW descr vname opname 1


{- Describes a testCase which must be tested manually. The result of the manual check must be given as first argument. -}
testManuallyW :: String -> Float -> Bool -> TestCase
testManuallyW descr points bool = buildTest ("MANUAL Check for " ++ descr) points
      "manual test not validated" bool

testManually descr = testManuallyW descr 1


{- tests if two sets are identical -}
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

{- builds a TestCaseResult based on a TestCase -}
testResult :: TestCase -> TestCaseResult
testResult tc = TestCaseResult (testIndex tc) -- index
                ((if tp then "[OK] " else "[--] ")
                  ++ testDescr tc ++ ": " ++
                  putResult tp (testError tc)) -- message
                (if tp then testPoints tc else 0) -- score
                False -- is undefined
                False -- is excepted
  where
    tp = testPassed tc
    putResult p e = if p then "PASSED" else "FAILED: " ++ e
            

{- runs a test case, handling a potential exception. -}
runTestCase :: Bool -> Int -> TestCase -> IO TestCaseResult
runTestCase showPoints nEx tc = do
  E.catch (do
             putStr $ if testPassed tc then "" else "" -- force generating exception    
             return $ let res = testResult tc in
                    res { testMessage = showEx showPoints nEx (testIndex tc) ++ testMessage res ++
                      (if testPassed tc && showPoints then " +" ++ showPt (testScore res) else "") } )
          (\ (E.SomeException e) -> do
             return $
              TestCaseResult 
                  (testIndex tc) -- index
                  (showEx showPoints nEx (testIndex tc) ++ "[-/] " ++ testDescr tc 
                    ++ ": EXCEPTION: " ++ (intercalate "\n\t\t" $ map (drop 1) $ groupBy (\a b -> b /= '\n') $ " " ++ show e)) -- message
                  0 -- score
                  (take (length "Prelude.undefined") (show e) == "Prelude.undefined")
                  True) -- excepted

{- runs a set of testData. The first 3 arguments define if vmchecker mode should be active, if points should be shown and how many undefined exercises to show. -}
runTests :: Bool -> Bool -> Int -> [TestData] -> IO ()
runTests checkerMode showPoints showDefaults tests = do
  results <- mapM (mapM (runTestCase showPoints $ length tests) . getTestCases) tests
  let 
    res = foldl showResult ([], []) results 
    in mapM putStr (reverse (fst res) ++
      if checkerMode || null (snd res) then []
      else ["Exercises not done: " ++ (show $ reverse $ snd res) ++ "\n"])
  printFinalGrade results showPoints
  where
    showResult (out, notdone) r = 
        if not checkerMode && isUndefined && length notdone >= showDefaults
                  then (out, notdoneNew)
                  else (concatMap printRes r : out, notdoneNew)
      where
        isUndefined = (and $ map testExcepted r) && (or $ map testSubjectUndefined r)
        notdoneNew = if isUndefined then testExercise (head r) : notdone else notdone
        printRes r = (if checkerMode then "+" ++ showPt (testScore r) ++ " " else "")
             ++ testMessage r ++ "\n"
    printFinalGrade rr True = do
      putStr $ "Final grade: "
      putStrLn $ showPt $ sum $ concat $ map (map testScore) rr
    printFinalGrade rr False = do
      putStrLn ""

{- use this for a laboratory situation; prints only the first 2 of the undefined exercises -}
quickCheck :: Bool -> [TestPP.TestData] -> IO ()
quickCheck showPoints = runTests False showPoints 2

{- use this for the vmchecker; prepends each test case with the points accumulated -}
vmCheck :: [TestPP.TestData] -> IO ()
vmCheck = runTests True True undefined