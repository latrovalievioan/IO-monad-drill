import System.IO.Unsafe
-- What is a side effect?
-- Something that modifies the world

data World = World deriving Show

f :: World -> World
f = undefined

printStr :: String -> World -> World
printStr s !w = unsafePerformIO (putStrLn s >> return w)

readStr :: World -> (String, World)
readStr !w = unsafePerformIO (getLine >>= (\s -> return (s, w)))

whatIsYourName :: IO ()
whatIsYourName = do
  putStrLn "What is your Name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)

whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
  where w2         = printStr "What is your name?" w1
        (name, w3) = readStr w2  
        w4         = printStr ("Hello, " ++ name) w3

-- this is a problem because the same World is used to
-- generate two different Worlds
-- a world should be used only once
branch :: World -> (World, World)
branch w = (
  printStr "I love you!" w,
  printStr "I hate you!" w
  )

-- Solving the Branching problem
-- Method 1: Uniqueness typing aka having unique types
-- enforced by compiler, implemented well in Clean programming language
--
-- Method 2: Make World inaccessable aka Hide the World
type WorldTransformer a = World -> (a, World)

printStrT :: String -> WorldTransformer ()
printStrT s w = ((), printStr s w)

readStrT :: WorldTransformer String
readStrT = readStr

bind :: WorldTransformer a            -- World -> (a, World)
        -> (a -> WorldTransformer b)    -- a -> World -> (b, World)
                                            --  If we apply uncurry: (a, World) -> (b, World)
                                            --  Notice how the (a, World) is the same in line 1 and 2
        -> WorldTransformer b           -- World -> (b, World)
bind wt f = uncurry f . wt

whatIsYourPureNameT :: WorldTransformer ()
whatIsYourPureNameT = 
  bind (printStrT "What is your name?") (\_ -> bind readStrT (\name -> printStrT ("Hello, " ++ name)) )
