{-# LANGUAGE InstanceSigs #-}
module T1 where


-- T1, from IO class 050

-- Define na new polymorphic data type Employee for represneting
-- and employee using /record syntax/. The record should have 4 fields"
-- firstName: string / lastName: string / salary: Double, 
-- and info: polymorphic type @a@ 
-- Derive 'Show' 'Eq' 'Ord' and 'Read' instances for this type

data Employee a = Employee
    {      -- add appropriate fields to this record type!
        firstName   :: String
    ,   lastName    :: String
    ,   salary      :: Double
    ,   info        :: a 
    } deriving (Show, Eq, Ord, Read)


-- write a function that computes the /full name/ of an Employee
-- firstname and lastname, separated by a space
-- >> fullName $ Employee "Charles" "Hoskinson" 1000 True
-- "Charles Hoskinson"

fullName :: Employee a -> String
fullName e = firstName e ++ " " ++ lastName e

-- double the salary of an Employee, not changing any of the other three fields
-- Use RECORD UPDATE syntax!

-- >>doubleSalary $ Employee "Charles" "Hoskinson" 1000 "info"
-- Employee {firstName = "Charles", lastName = "Hoskinson", salary = 2000.0, info = "info"}


-- Error:
-- No instance for (Show (Employee [Char]))
-- REQUIRES SHOW!

doubleSalary :: Employee a -> Employee a
-- create a new record by using an existing one and just updating certain fields
doubleSalary e = e {salary = 2 * salary e}

-- make Employee an instance of class Functor
-- >>> not <$> Employee "Charles" "Hoskinson" 1000 True
-- Employee {firstName = "Charles", lastName = "Hoskinson", salary = 2000.0, info = False}


-- you should not have a type signature here for this function
-- unless you use an extension, pragma
-- {-# LANGUAGE InstanceSigs #-}

instance Functor Employee where
    -- fmap = error "Implement fmap!
    -- fma
    fmap :: (a -> b) -> Employee a -> Employee b
    --fmap f e = error "Implement fmap!"
    
    -- Given a function f from a to b, and an Employee a
    -- we must produce a new Employee with info of type b
    -- for strings and numbers, the obvious choice is to leave them
    -- with firstName, lastName, salary and info of type b
    -- but fn, ln and salary, we just copy from employee given e.
    -- and info? we dont know anything about types a and b, 
    -- so the only way to produce b is by applying the function f to a
    -- and the only a we have is the info field of e
    fmap f e = e {info = f (info e)}


-- forgetInfo
-- forgets the info but keeps all other info untouched
-- USING fmap!
-- >> forgetInfo (Employee "Charles" "Hoskinson" 1000 True)
--  Employee {firstName = "Charles", lastName = "Hoskinson", salary = 2000.0, info = ()}

forgetInfo :: Employee a -> Employee ()
-- () is unit, there is only one way to implement a function that returns unit
-- the following lambda always returns ()

--forgetInfo e  = fmap (\a -> ()) e
--forgetInfo e  = fmap (\_ -> ()) e     -- Because we dont care on the input
--forgetInfo e  = fmap (const ()) e     -- because const always returns the second parameter, ()
forgetInfo      = fmap (const ())     -- now doing reduction, removing e from left and right



--gatherInfo
-- takes a list of employees and extracts their info
-- the resulting list should contain the info of each employee 
-- in the argument list in the same order
-- >>> :{
--        gatherInfo [ Employee "Charles" "Hoskinson" 1000 True
--                  Employee "Alejandro" "Garcia" 900 False
--                ]
-- }
-- [True, False]



gatherInfo :: [Employee a] -> [a]
-- gatherInfo e = map info e           -- !!!
gatherInfo = map info                   -- reduction
-- the function info extracts the info from the employee 

-- a type for /rose trees/
data Rose a = Fork a [Rose a] deriving Show

-- e = Fork 'x' [Fork 'y' [], Fork 'z' []]


-- collect all data from a rose tree in a list (in /pre-order/)
-- i.e from top to bottom

roseToList :: Rose a -> [a]
-- roseToList (Fork x []) = [x]            -- if there is no child, then just [x]

-- NOT THIS: 
-- roseToList (Fork x xs) = x : map roseToList xs
-- BUT this above results in a list of list of list of as
-- we just want 1 list

-- there is a function 'concatMap' that does this! this is one solution

-- another solution: 'concat' turn a list of list of as into a list of as
-- roseToList (Fork x xs) = x : concat (map roseToList xs)
-- >>concat [[1,2,3], [4,6], [7,8,9]]
-- [1,2,3,4,6,7,8,9]

-- >> map (\n -> [n, n+1]) [1,2,3]
-- [[1,2],[2,3],[3,4]]


--  >> concatMap (\n -> [n, n+1]) [1,2,3]
-- [1,2,2,3,3,4]


-- if you dont know the previous 2 functions, then you can write it yourself: 
-- -> concat'

roseToList (Fork x xs) = x : concat' (map roseToList xs)  
    where
        concat' :: [[a]] -> [a]             --list of list of as into list of as
        concat' []         = []
        concat' (ys : yss) = ys ++ concat' yss


-- sumRose
sumRose :: Num a => Rose a -> a
sumRose = sum . roseToList
-- simple concatenation of 2 functions
-- convert to list
-- then use simple sum function