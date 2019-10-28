module Test (runTest) where

runTest :: [Bool] -> String
runTest x = if all (id) x
            then "Test succeeded"
            else "Test failed"
