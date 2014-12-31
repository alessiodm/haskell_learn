import qualified Data.Map as Map

--data Bool = False | True
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float |
             Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
        (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)

data Triple t = Triple { first :: t
                       , second :: t
                       , third :: t
                       } deriving (Show)


  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- list
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


-- tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


-- Emulating JavaScript
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where  
    yesno = id -- Id takes a parameter and return itself

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf x yescase nocase = if yesno x then yescase else nocase

-- instance Functor Maybe where
--      fmap f (Just a) = Just (f a)
--      fmap f Nothing = Nothing

class Tofu t where  
    tofu :: j a -> t a j 
