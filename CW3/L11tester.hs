-- Lambda expression: (\x -> x + 1) 3
-- Environment type: a mapping of variable names to values
type Environment = [(String, Int)]
-- Function to perform beta reduction
betaReduce :: Environment -> String -> Int -> Int
betaReduce env var value = case lookup var env of
 Just oldValue -> value + oldValue
 Nothing -> value
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- Example lambda expression
lambdaExpr :: Int -> Int
lambdaExpr = (\x -> x + 1)

-- A simple function that adds two numbers
addTwo :: Int -> Int -> Int
addTwo x y = x + y
-- A function that takes two numbers, adds them, and applies acontinuation function to the result
addWithContinuation :: Int -> Int -> (Int -> a) -> a
addWithContinuation x y continuation = continuation (addTwo x y)
-- Example continuation function: multiply the result by 2
multiplyByTwo :: Int -> Int
multiplyByTwo result = result * 2

-- Example usage
main :: IO ()
main = do
    let x = 3
        y = 4
-- Using addWithContinuation with multiplyByTwo as thecontinuation
        result = addWithContinuation x y multiplyByTwo
        result2 = addWithContinuation x y (\res -> res * 2)
    putStrLn $ "Result:" ++ show result
    putStrLn $ "Result2:" ++ show result2

