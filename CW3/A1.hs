{-# LANGUAGE LambdaCase #-}
import Data.Data (ConIndex)
type Environment = [(String, Expr)]
data Expr = Var String | Lam String Expr | App Expr Expr | Closure String Expr Environment deriving (Eq,Show,Read)
-- Types for Frames and Continuations
data Frame = VarFrame String | AppFrame Expr Environment deriving (Eq, Show, Read)

type Continuation = [Frame]

-- Configuration type
data Configuration = Configuration
  { control :: Expr,        -- Current control
    environment :: Environment,  -- Environment
    continuation :: Continuation  -- Continuation
  } deriving (Eq, Show, Read)
-- data Configuration = Configuration (Expr,Environment,Continuation) deriving (Eq, Show, Read)
-- data Configuration = {VarConfrg Expr | EnvironmentConfrg Environment | ContinuationConfg Continuation }deriving (Eq, Show, Read)

lookupFunction :: String -> Environment -> Maybe Expr
lookupFunction name ((var,expr):env) = case lookup name ((var,expr):env)  of
    Just name -> Just expr
    Nothing -> Nothing

-- Sample environments for testing
env1 :: Environment
env1 = [("x", Var "x"), ("y", Lam "z" (Var "z"))]

env2 :: Environment
env2 = [("a", Lam "x" (App (Var "x") (Var "x"))), ("b", App (Var "a") (Lam "y" (Var "y"))) ]

eval1 :: Configuration -> Configuration
-- eval1cbv :: Expr -> Expr 
-- eval1cbv (Lam x e) = (Lam x e)
-- eval1cbv (App (Lam x e1) e@(Lam y e2)) = subst e1 x e 
-- eval1cbv (App e@(Lam x e1) e2) = App e (eval1cbv e2) 
-- eval1cbv (App e1 e2) = App (eval1cbv e1) e2
-- Assume Expr, Environment, and Configuration are defined as in previous responses


-- -- Rule R1: Beta-reduction for function application
-- eval1 (Configuration (App (Lam x e1) e2) env k) =Configuration e1 (extendEnv x e2 env) k
-- -- Rule R2: Evaluate the function in an application
-- eval1 (Configuration (App e1 e2) env k) =Configuration e1 env (AppFrame e2 env : k)
-- -- Rule R3: Variable lookup
-- eval1 (Configuration (Var x) env k) = case lookupFunction x env of
--         Just expr -> Configuration expr env k
--         Nothing   ->Configuration (Var x) env k
-- -- Rule R4: Apply the function in an application
-- eval1 (Configuration v env (AppFrame e env' : k)) =Configuration (App v e) env' k
-- -- Rule R5: Successful termination
-- eval1 (Configuration v env []) =Configuration v env []  -- Successful termination
-- -- Catch-all for other cases
-- eval1 _ = error "No applicable reduction rule"


-- Reduction rules (R1-R5)
-- Rule R1: Beta-reduction for function application
-- eval1 (Configuration (App (Lam x e1) e2) env k) = Configuration e1 (extendEnv x e2 env) k  -- R1
-- -- Rule R2: Evaluate the function in an application
-- eval1 (Configuration (App e1 e2) env k) = Configuration e1 env (AppFrame e2 env : k)  -- R2
-- eval1 (Configuration (Var x) env k) = case lookupFunction x env of
--         Just expr -> Configuration expr env k  -- R3
--         Nothing   -> Configuration (Var x) env k
-- eval1 (Configuration v env (AppFrame e env' : k)) = Configuration (App v e) env' k  -- R4
-- eval1 (Configuration v env []) = Configuration v env []  -- Successful termination (R5)
-- eval1 _ = error "No applicable reduction rule"  -- Catch-all for other cases

eval1 (Configuration (App (Lam x e1) e2) env k) = Configuration (App (Lam x e1) e2) (extendEnv x e2 env) k  -- R1
-- Rule R2: Evaluate the function in an application
eval1 (Configuration (App e1 e2) env k) = Configuration (App e1 e2) env (AppFrame e2 env : k)  -- R2
eval1 (Configuration (Var x) env k) = case lookupFunction x env of
        Just expr -> Configuration expr env k  -- R3
        Nothing   -> Configuration (Var x) env k
eval1 (Configuration v env (AppFrame e env' : k)) = Configuration (App v e) env' k  -- R4
eval1 (Configuration v env []) = Configuration v env []  -- Successful termination (R5)
eval1 _ = error "No applicable reduction rule"  -- Catch-all for other cases

-- Rule R1: Beta-reduction for function application
-- eval1 (Configuration (App (Lam x e1) e2) env k) = Configuration e2 (extendEnv x e1 env) k  -- R1
-- -- Rule R2: Evaluate the function in an application
-- eval1 (Configuration (App e1 e2) env k) = Configuration e2 env (AppFrame e1 env : k)  -- R2
-- eval1 (Configuration (Var x) env k) = case lookupFunction x env of
--         Just expr -> Configuration expr env k  -- R3
--         Nothing   -> Configuration (Var x) env k
-- eval1 (Configuration v env (AppFrame e env' : k)) = Configuration (App v e) env' k  -- R4
-- eval1 (Configuration v env []) = Configuration v env []  -- Successful termination (R5)
-- eval1 _ = error "No applicable reduction rule"  -- Catch-all for other cases

-- Helper function to extend the environment
extendEnv :: String -> Expr -> Environment -> Environment
extendEnv x expr env = (x, expr) : env

config1 :: Configuration
config1 = Configuration (Var "x") env1 []

config2 :: Configuration
config2 = Configuration (App (Lam "x" (Var "x")) (Var "y")) env1 []

config3 :: Configuration
config3 = Configuration (App (Var "a") (Var "b")) env2 []

config4 :: Configuration
config4 = Configuration (Var "result") env1 []

expr3 =App (Lam "v"
                  (App
                    (App (Var "v") (Lam "z" (Var "z")))
                    (App (Lam "v" (App
                        (App (Var "v") 
                            (Lam "x" (Lam "y" (Var "x") )
                            )
                        ) 
                        (Lam "z" (Var "z")))
                        )
                        (Lam "x" (Lam "y" (Var "x")
                        ))
                    )
                  )
                 )
                (Lam "x" (Lam "y" (Var "y")))

config5 = Configuration  (Lam "x" (Lam "y" (Var "x"))) env1 []

-- Initial configuration setup
initialConfig :: Expr -> Configuration
initialConfig expr = Configuration expr [] []

-- -- Eval function
-- eval :: Expr -> Expr
-- eval expr = eval' (initialConfig expr)
--   where
--     eval' :: Configuration -> Expr
--     eval' config =
--       case eval1 config of
--         newConfig@(Configuration _ _ []) -> control newConfig  -- Termination condition

--         newConfig -> eval' newConfig

-- Eval function
eval :: Expr -> Expr
eval expr = eval'' (eval' (initialConfig expr))
  where
    eval' :: Configuration ->Configuration
    eval' config =
      case eval1 config of
        newConfig@(Configuration _ _ []) -> newConfig  -- Termination condition                                       
        newConfig -> eval' newConfig

eval'' :: Configuration ->Expr
eval'' config =
      case eval2 config of
        newConfig@(Configuration _ _ []) ->  control newConfig  -- Termination condition                                       
        newConfig -> eval'' newConfig

-- eval2 (Lam x e) =Lam x e
-- eval2 (App (Lam x e1) e2)  = App e1 (eval2 e2)
-- eval2 (App e1 e2)  =  App (eval2 e1) e2
-- eval2 (Var x) = Var x
-- eval2 (App (Lam x e1) e@(Lam y e2))= subst e1 x 32
eval2 (Configuration (App (Lam x e1) e2) env k) = Configuration e2 (extendEnv x e1 env) k  -- R1
-- Rule R2: Evaluate the function in an application
eval2 (Configuration (App e1 e2) env k) = Configuration e2 env (AppFrame e1 env : k)  -- R2
eval2 (Configuration (Var x) env k) = case lookupFunction x env of
        Just expr -> Configuration expr env k  -- R3
        Nothing   -> Configuration (Var x) env k
eval2 (Configuration v env (AppFrame e env' : k)) = Configuration (App v e) env' k  -- R4
eval2 (Configuration v env []) = Configuration v env []  -- Successful termination (R5)
eval2 _ = error "No applicable reduction rule"  -- Catch-all for other cases

-- Test cases
main :: IO ()
main = do
  -- Test case 1: Variable found in the environment
  putStrLn "Test Case 1:"
  print $ lookupFunction "x" env1  -- Should print: Just (Var "x")

  -- Test case 2: Variable not found in the environment
  putStrLn "\nTest Case 2:"
  print $ lookupFunction "z" env1  -- Should print: Nothing

  -- Test case 3: Variable found in a complex environment
  putStrLn "\nTest Case 3:"
  print $ lookupFunction "a" env2  -- Should print: Just (Lam "x" (App (Var "x") (Var "x")))

  -- Test case 4: Variable not found in a complex environment
  putStrLn "\nTest Case 4:"
  print $ lookupFunction "c" env2  -- Should print: Nothing

  -- Test case 1: Variable lookup
  putStrLn "Test Case 1:"
  print $ eval1 config1

  -- Test case 2: Application with a lambda
  putStrLn "\nTest Case 2:"
  print $ eval1 config2

  -- Test case 3: Application with a variable
  putStrLn "\nTest Case 3:"
  print $ eval1 config3

  -- Test case 4: Successful termination
  putStrLn "\nTest Case 4:"
  print $ eval1 config4

  let expr1 = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))
  let expr2 = App (Lam "x" (Var "x")) (App (Lam "y" (Var "y")) (Var "z"))

  putStrLn "Eval Result 1:"
  print $ eval expr1
  -- Expected output: Lam "y" (Var "y")

  putStrLn "\nEval Result 2:"
  print $ eval expr2
  -- Expected output: App (Lam "y" (Var "y")) (Var "z")

  print $ eval1 config5
  let expr3 =App (Lam "v"
                  (App
                    (App (Var "v") (Lam "z" (Var "z")))
                    (App (Lam "v" (App
                        (App (Var "v") 
                            (Lam "x" (Lam "y" (Var "x") )
                            )
                        ) 
                        (Lam "z" (Var "z")))
                        )
                        (Lam "x" (Lam "y" (Var "x")
                        ))
                    )
                  )
                 )
                (Lam "x" (Lam "y" (Var "y")))
  putStrLn "\nEval Result 3:"
  print $ eval expr3

  putStrLn "\nEval Result 3:"
  print $ eval'' (initialConfig expr3)