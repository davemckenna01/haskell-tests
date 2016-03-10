data McKenna = Kev | Tom | Dave deriving Show

data Listything a = Nothingness | Prependerthingy a (Listything a) deriving (Show, Read, Eq, Ord)