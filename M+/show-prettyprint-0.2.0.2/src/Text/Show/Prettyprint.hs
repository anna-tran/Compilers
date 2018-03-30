-- | Format a 'show'-generated string to make it nicer to read.
--
-- For example, consider this nested data structure:
--
-- >>> :{
-- let nestedExample = fromList
--         [ ("hello", Left  (Pair True ()))
--         , ("world", Right (Record { r1 = ('c', -1.2e34), r2 = 123 }))
--         , ("!"    , Left  (Pair False ())) ]
-- :}
--
-- Applying 'show' to it results in the fairly dense representation
--
-- >>> print nestedExample
-- fromList [("!",Left (Pair False ())),("hello",Left (Pair True ())),("world",Right (Record {r1 = ('c',-1.2e34), r2 = 123}))]
--
-- With the functions defined in this module, we can make this output a bit more
-- readable,
--
-- >>> prettyPrint nestedExample
-- fromList [("!",Left (Pair False ()))
--          ,("hello",Left (Pair True ()))
--          ,("world",Right (Record {r1 = ('c',-1.2e34),r2 = 123}))]
module Text.Show.Prettyprint (
    prettifyShow,
    prettyShow,
    prettyPrint,
) where



import Text.Trifecta

import Text.Show.Prettyprint.Internal



-- $setup
-- >>> data Record a b = Record { r1 :: a, r2 :: b } deriving Show
-- >>> data Pair a b = Pair a b deriving Show
-- >>> import Data.Map (fromList)



-- | Prettyprint a string produced by 'show'. On parse error, silently fall back
-- to a non-prettyprinted version.
prettifyShow :: String -> String
prettifyShow s = case parseString shownP mempty s of
    Success x -> show x
    Failure _ -> s

-- | 'prettifyShow' with the 'show' baked in.
prettyShow :: Show a => a -> String
prettyShow = prettifyShow . show

-- | 'prettifyShow' with the 'show' and the 'putStrLn' baked in.
prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . prettyShow
