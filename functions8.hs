-- Functors and monoids
import Data.Char  
import Data.List  
  
--main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
--          putStrLn line

class (Functor f) => Applicative' f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' [] where
    pure x = [x]
    funcs <*> values = [ f v | f <- funcs, v <- values]

