module Test (runTest) where

runTest :: [Bool] -> String
runTest [] = "No test cases"
runTest x = if all (id) x
            then "Test succeeded"
            else "Test failed"
