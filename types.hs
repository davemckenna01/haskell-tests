-- a polymorphic function. "a" is a placeholder for any type, and is called a
-- "type variable". Type var in signature = a polymorphic fn
len :: [a] -> Int
len [] = 0
len (x:xs) = length xs + 1


-- "Char" is a "concrete type"
dave :: [Char]
dave = "Dave"


-- repeated type variables always represent the same type
head' :: [a] -> a
head' (x:xs) = x


-- sometimes you need to add constraints to your type variables
-- b/c sum' :: [a] -> a won't work, b/c summing non-numbers makes no sense
sum' :: Num a => [a] -> a
-- the double arrow indicates a constraint on the type variable
sum' [] = 0
sum' (x:xs) = x + sum xs

-- Num is a "type class". It represents all types which can do something.
-- In this case, all types which can do numeric things.

-- Another example:
show' :: Show a => a -> [Char]
-- show is kinda like Haskell's toString. But only some types can be
-- represented as strings. For example fns can not. So we constrain that
-- only those that are part of the Show type class are allowable args.

-- can also have multiple type class constraints:
showSum :: (Num a, Show a) => [a] -> [Char]
showSum xs = show (sum xs)
-- so a must be a num to allow the sum, but also showable to allow the show

-- type synonyms
type Stringy = [Char]
type Point = (Double, Double)

-- ----------------------------------------------
-- ----------------------------------------------
-- Newtype
-- ----------------------------------------------
-- Newtype, creates a new type that is represented by an existing type
-- adds semntic meaning, checked by compiler

newtype CustomerId = CustomerId Int

-- this wont' work:
badCustomer :: CustomerId
badCustomer = 13
-- it won't work b/c you have to user the constructor
customer :: CustomerId
customer = CustomerId 13
-- and the only way to get the int out is to build a fn for it w/ patter matching:
customerToInt :: CustomerId -> Int
customerToInt (CustomerId i) = i
-- this pattern matches a customer id which was created by calling the
-- MakeCustomerId constructor

-- ----------------------------------------------
-- ----------------------------------------------
-- Algebraic Data Types
-- ----------------------------------------------
-- Algebraic Data Types are the workhorse of Haskell's type system
-- Almost every type you use in Haskell will be an algebraic data type
-- similar to Newtype, but support more args
-- ADTs with one constructor are similar to structs/classes
-- ADTs with multiple constructors have no parallel in OOP, they
-- allow you to have a type that can have different kinds of values

-- <data keyword> <name of type> <"constructor"* name>
-- <remaining are types of args to constructor: CustomerId, String, Int>
-- * not the same as OOP constructors
-- in Haskell they're just names that glue values together
-- the args are only identified by position, not by "field name"
data Customer = Customer CustomerId String Int

alice :: Customer
alice = Customer (CustomerId 13) "Alice" 42

-- you get values out by building util fns that use pattern matching
getCustomerId :: Customer -> CustomerId
getCustomerId (Customer cust_id name luckyNumber) = cust_id
-- can also do this when you don't need some args and don't want to
-- have to think of names:
getCustomerId (Customer cust_id _ _) = cust_id
-- if you wanted to show (print) the cust id in ghic:
customerToInt $ getCustomerId alice

-- algebraic data types are great for semantics:
data RGB = RGB Double Double Double
x' :: RGB
x' = RGB 1 2 3
-- is way better than:
x :: (Double, Double, Double)
x = (1, 2, 3)

-- ADTs with multiple constructors:
data Bool = False | True
x :: Bool
x = False
y :: Bool
y = True

-- another eg (eg of an Enum-like type):
data DialogeResponse = Yes | No | Help | Quit


-- and another eg
data MaybeInt = NoInt | JustInt Int

defaultInt :: Int -> MaybeInt -> Int
-- this pattern matches when the MaybeInt value was created with the
-- NoInt constructor
defaultInt defaultValue NoInt = defaultValue
-- this pattern matches when the MaybeInt value was created wth the
-- JustInt constructor with x as the parameter
defaultInt _ (JustInt x) = x

-- -------------------------------------------------
-- -------------------------------------------------
-- Parameterized types
-- -------------------------------------------------
-- Parameterized types are types with a type variable, eg:
 data Maybe a = Just a | Nothing
-- this indicates that Maybe itself is not a type, but if you supply 
-- some type for a, it can produce a type, eg:
 x :: Maybe Int
 x = Nothing
-- or, working with a polymorphic function:
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultValue Nothing = defaultValue
fromMaybe _ (Just x) = x



-- -------------------------------------------------
-- -------------------------------------------------
-- Type Classes
-- -------------------------------------------------

-- note the double equals. x and y both have to be in the "Eq" type class
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys)
  | x == y    = True
  | otherwise = elem x ys

-- Int and String are automatically in the Eq type class, and are
-- equality-testable
-- What if you want to do this:
data RGB = RBG Int Int Int
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem green colors
-- this won't work bc RGB type is not in the Eq type class
-- the solution is to create an instance of the Eq type class for the
-- RGB type:
instance Eq RGB where
    -- pattern matching again
    (RGB r1 g1 b1) == (RGB r2 g2 b2) =
        (r1 == r2) && (g1 == g2) && (b1 == b2)

-- this is like OOP interfaces, and like saying RGB implements the EQ
-- interface

-- another eg:
instance Show RGB where
    show (RGB r g b) =
        "RGB " ++ (show r) ++ (show g) ++ (show b)

-- and now with paramaterized types. In this example we have to make sure
-- that the  and y in the last line are equality testable, and so have
-- to include that CONSTRAINT on the type parameter in the first line:
-- reminder: data Maybe a = Nothing | Just a
instance (Eq a) => Eq (Maybe a) where
    Nothing  == Nothing  = True
    Nothing  == (Just _) = False
    (Just _) == Nothing  = False
    (Just x) == (Just y) = x == y

-- Note that we're not defining "not equal". Haskell performs some magic
-- where, when you define a type class, you can define default
-- implementations of the operations. It just so happens that Eq
-- defines defaults for == and /= that are defined in terms of each
-- other. So if you define one, the other automatically works for
-- your type. Eg for Eq: see below "Defining type classes"
-- For eq, we call defining one of == or /= the "minimum complete definition"

-- deriving is a way to avoid tedious writing out of obvious operation
-- definitions. Eg RGB's eq is the same as any other data type that
-- has 3 elements... etc.
data RGB = RGB Int Int Int
    deriving Eq

-- but deriving only works for a few type classes:
-- Eq, which derives component-wise equality
-- Ord, which is >, <, >=, <=. Derivies component-wise comparison
-- Show, which derives "{Constructor name} {arg1} {arg2} ..."
-- Read, parses output of default show

-- Defining type classes
-- this is roughly how Eq is defined in the std lib
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)

-- define your own when you're looking for overloading, which is
-- a function or operator that should work for several different
-- types but with different behaviour for each type.
-- TOO LAZY TO WRITE OUT EXAMPLE

-- Subclasses
-- eg, Ord is a subclass of Eq.

-- Here's the Ord type class definition. Note the constraint of
-- a being in the Eq type class
class (Eq a) => Ord a where
    (<)     :: a -> a -> Bool
    (>)     :: a -> a -> Bool
    (<=)    :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    compare :: a -> a -> Ordering
    max     :: a -> a -> a
    min     :: a -> a -> a


