module TicTacToe where

data Three = One | Two | Three deriving (Eq, Ord, Enum, Bounded)

-- data TicTac a = TicTac { board :: Three -> Three -> a }

-- emptyBoard :: TicTac (Square 'E)
-- emptyBoard = TicTac $ const $ const $ Square E

newtype Square (a :: Spot) = Square Spot deriving (Show, Eq)

empty :: Square 'E
empty = Square E

x :: Square 'X
x = Square X

o :: Square 'O
o = Square O


-- koz_  ::  So what you need to do is cook a new function which takes a tuple, checks if it matches the argument; if it does, return
--                 ::  whatever the user requested we put there, otherwise delegate back to the original board function.

data Spot = X | O | E deriving (Show, Eq)
data TicTac a = TicTac { board :: (Three, Three) -> a }

-- makeMove :: TicTac Spot -> (Three, Three) -> Spot -> Maybe (TicTac Spot)
-- makeMove tictac move xo = case board tictac move of
--     X -> Nothing
--     E -> Just $ TicTac $ \(one, two) -> 
--     O -> 

