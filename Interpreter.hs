module Interpreter
  (
    -- * Types
    Prog,
    Asgn,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where

-------------------------------------------------------------------------------
--------------------------------- The Expr ADT  -------------------------------
-------------------------------------------------------------------------------
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

-- TODO Implement a parser for the Expr ADT.
--

-------------------------------------------------------------------------------
---------------------------------- The Prog ADT -------------------------------
-------------------------------------------------------------------------------
data Asgn = Asgn String Expr deriving (Show, Read)

data Prog = Eq Asgn
          | Seq Prog Prog
          | If Expr Prog Prog
          | For Asgn Expr Asgn Prog
          | Assert Expr
          | Return Expr deriving (Show, Read)

-- TODO Implement a parser for the Prog ADT.
--

-- TODO The *parse* function. It receives a String - the program in
-- a "raw" format and it could return *Just* a program as an instance of the
-- *Prog* data type if no parsing errors are encountered, or Nothing if parsing
-- failed.
--
-- This is composed with evalAdt to yield the evalRaw function.
parse :: String -> Maybe Prog
parse = undefined

-------------------------------------------------------------------------------
-------------------------------- The Interpreter ------------------------------
-------------------------------------------------------------------------------

-- TODO The *evalAdt* function.  It receives the program as an instance of the
-- *Prog* data type and returns an instance of *Either String Int*; that is,
-- the result of interpreting the program.
--
-- The result of a correct program is always an Int.  This is a simplification
-- we make in order to ease the implementation.  However, we wrap this Int with
-- a *Either String* type constructor to handle errors.  The *Either* type
-- constructor is defined as:
--
-- data Either a b = Left a | Right b
--
-- and it is generally used for error handling.  That means that a value of
-- *Left a* - Left String in our case - wraps an error while a value of *Right
-- b* - Right Int in our case - wraps a correct result (notice that Right is a
-- synonym for "correct" in English).
-- 
-- For further information on Either, see the references in the statement of
-- the assignment.
--

--type used for storing values of type (String, Int) in order to map a variable
--to its specific value; further referred to as variable list
type Values = [(String, Int)]

-- the result of a program evaluation: String if an error has occured and Int for
-- the result of a correct operation; Values for passing the eventually modified
-- variable list to the following program; Bool to signal if we have encountered
-- a return or an error, case in which the following programs don't need to be 
-- evaluated and the error which was first encountered will be returned
type Res = (Either String Int, Values, Bool)

--checks if the given variable is part of the variable list received as parameter
checkScope :: String -> Values -> Bool
checkScope s [] = False
checkScope s (h:t) = if (s == (fst h)) then True
                                        else checkScope s t

-- gets the value for a given variable from the variable list received as parameter
getValue :: String -> Values -> Int
getValue s [] = undefined
getValue s (h:t) = if (s == (fst h)) then snd h
                                     else getValue s t

-- evaluates an Add expression in the following manner: evaluate the first expression
-- receiver by Add; if it evaluates to an error pass it further on ; otherwise evaluate
-- the second expression received by Add: if it evaluates to an error pass it further on,
-- otherwise return the sum of the results of the two evaluaions
evalAdd :: Expr -> Values -> Either String Int
evalAdd (Add a b) values = case (evalExpr a values) of
                    Left msg -> Left msg
                    Right val -> case (evalExpr b values) of
                                                    Left msg -> Left msg
                                                    Right valb -> Right((+) val valb)

-- evaluates a Mult expression in the following manner: evaluate the first expression
-- receiver by Mult; if it evaluates to an error pass it further on ; otherwise evaluate
-- the second expression received by Mult: if it evaluates to an error pass it further on,
-- otherwise return the product of the results of the two evaluaions
evalMult :: Expr -> Values -> Either String Int
evalMult (Mult a b) values = case (evalExpr a values) of
                    Left msg -> Left msg
                    Right val -> case (evalExpr b values) of
                                                    Left msg -> Left msg
                                                    Right valb -> Right((*) val valb)

-- evaluates a Sub expression in the following manner: evaluate the first expression
-- receiver by Sub; if it evaluates to an error pass it further on ; otherwise evaluate
-- the second expression received by Sub: if it evaluates to an error pass it further on,
-- otherwise return the difference of the results of the two evaluaions
evalSub :: Expr -> Values -> Either String Int
evalSub (Sub a b) values = case (evalExpr a values) of
                    Left msg -> Left msg
                    Right val -> case (evalExpr b values) of
                                                    Left msg -> Left msg
                                                    Right valb -> Right((-) val valb)

-- evaluates an Equal expression in the following manner: evaluate the first expression
-- receiver by Equal; if it evaluates to an error pass it further on ; otherwise evaluate
-- the second expression received by Equal: if it evaluates to an error pass it further on,
-- otherwise return True if the 2 results are equal and False otherwise
evalEqual :: Expr -> Values -> Either String Bool
evalEqual (Equal a b) values = case (evalExpr a values) of
                    Left msg -> Left msg
                    Right val -> case (evalExpr b values) of
                                                    Left msg -> Left msg
                                                    Right valb -> Right((==) val valb)

-- evaluates an Smaller expression in the following manner: evaluate the first expression
-- receiver by Smaller; if it evaluates to an error pass it further on ; otherwise evaluate
-- the second expression received by Smaller: if it evaluates to an error pass it further on,
-- otherwise return True if the first value is smaller than the second and False otherwise
evalSmaller :: Expr -> Values -> Either String Bool
evalSmaller (Smaller a b) values = case (evalExpr a values) of
                    Left msg -> Left msg
                    Right val -> case (evalExpr b values) of
                                                    Left msg -> Left msg
                                                    Right valb -> Right((<) val valb)

-- evluates a given expression by checking its type and calling the appropriate
-- function to evaluate it; works for expressions which don't return a Bool value
-- the evaluation of a value is the value itself
-- for evaluating a Symbol we check if it is part of the variable list received
-- as a parameter: if no, then an "Uninitialized variable" error occurs; otherwise
-- the coresponding value for the variable is returned
evalExpr :: Expr -> Values -> Either String Int
evalExpr expr val = case expr of
                    Value a -> (Right a)
                    Symbol a -> if ((checkScope a val) == False) then (Left ("Uninitialized variable"))
                                                                 else (Right (getValue a val))
                    Add a b -> (evalAdd expr val)
                    Mult a b -> (evalMult expr val)
                    Sub a b -> (evalSub expr val)

-- evluates a given expression by checking its type and calling the appropriate
-- function to evaluate it; works for expressions which return a Bool value
evalExprBool :: Expr -> Values -> Either String Bool
evalExprBool expr val = case expr of
                    Equal a b -> (evalEqual expr val)
                    Smaller a b -> (evalSmaller expr val)

-- receives an Asgn program and a variable and evaluates an Asgn programm in the 
-- following manner: evaluate the expression received by Asgn (we know that this 
-- expression can't return a Bool, so we use the evalExpr function) and check the 
-- result: if an error has occured then pass it further on; otherwise introduce in
-- the variable list a pair consisting in the given variable and the previously
-- obtained result
evalAsgn :: Asgn -> Values -> Res
evalAsgn (Asgn var expr) values = case (evalExpr expr values) of
                                  Left msg -> (Left msg, values, True)
                                  Right val -> (Right val, ((var, val):values), False)

-- receives an Eq programm and a variable list and evaluates an Eq program by calling
-- evaluation on the Asgn program received by Eq and returning the same result
evalEq :: Prog -> Values -> Res
evalEq (Eq asgn) values = evalAsgn asgn values

-- receives a Seq program and a variable list and evaluates the Seq program in the following
-- manner: evaluate the first program: if an error has occured then pass it on;
-- otherwise look at the Bool variable in the result: if it is True, then we came
-- across a return in the previous evaluation, so we will pass on the same value,
-- without evaluating the second program. If the Bool variable in the result of
-- the first program is False then evaluate the second program: if it produces
-- an error then pass it on; otherwise return the result obtained by the second evaluation.
evalSeq :: Prog -> Values -> Res
evalSeq (Seq p1 p2) values = case (evalAdtaux p1 values) of
                              (Left msg, _, f) -> (Left msg, values, True)
                              (Right val1, newVal, False) -> case (evalAdtaux p2 newVal) of
                                                            (Left msg, _ ,f) -> (Left msg, values, True)
                                                            (Right val2, finalVal, b) -> (Right val2, finalVal, b)
                              (Right val1, newVal, True) -> (Right val1, newVal, True)
                                                        
-- receives an If program and a variable list and evaluates the If program in the
-- following manner: evaluate the condition of the if (which returns a Bool, so we
-- use the evalExprBool function): if an error has occured then pass it on; otherwise 
-- check the result: if it was True, then evaluate the first program received by If
-- else evaluate the second program received by If
evalIf :: Prog -> Values -> Res
evalIf (If expr prog1 prog2) values = case (evalExprBool expr values) of
                                      Left msg -> (Left msg, values, True)
                                      Right val -> case val of
                                                   True -> evalAdtaux prog1 values
                                                   False -> evalAdtaux prog2 values 

-- receives a result, a variable list and an Asgn program: evaluates the Asgn
-- program and in case any error has occured it passes it along; otherwise it
-- checks the Bool value return by the Asgn evaluation: if it is true, then the
-- fuction returns the newly computed value returned by Asgn; otherwise it returns
-- the Res received as parameter;
-- this function is used for doing the last assignment when exiting a for loop
lastAssign :: Res -> Values -> Asgn -> Res                                   
lastAssign (Right v, valacc, end) values asgn = case (evalAsgn asgn values) of
                                                  (Left msg, _, f) -> (Left msg, values, True)
                                                  (Right val, newVal, True) -> (Right val, newVal, True)
                                                  (Right val, newVal, False) -> (Right v, newVal, False)

-- receives an accumulator for the result, a condition, a program to be
-- evaluated, a list variable and an assignment;
-- this auxiliary function simulates the execution of a program inside a for loop:
-- evaluate the assignment and pass on any errors; if a return value has been reached
-- during evaluation pass it on; otherwise evaluate the condition: if any error has
-- occured then pass it further on; ontherwise check the result: if it is False then do
-- one more assignment using the lastAssign function and return the result;
-- if the condition is True, then evaluate the program and check the result:
-- pass on any errors or returns; if none has occured, then recursively call the
-- function, with the newly computed value for the program as accumulator
auxFor2 :: Res-> Expr -> Prog -> Values -> Asgn -> Res
auxFor2 acc cond prog values as2 = case (evalAsgn as2 values) of
                                  (Left msg, _, f) -> (Left msg, values, True)
                                  (Right val, newVal, False) -> case (evalExprBool cond newVal) of
                                                                Left msg -> (Left msg, newVal, True)
                                                                Right valeval -> case valeval of
                                                                              True -> case (evalAdtaux prog newVal) of
                                                                                      (Left msg, _, f) -> (Left msg, values, True)
                                                                                      (Right val2, newVal2, False) -> auxFor2 (Right val2, newVal2, False) cond prog newVal2 as2
                                                                                      (Right val2, newVal2, True) -> (Right val2, newVal2, True)
                                                                              False -> lastAssign acc values as2
                                  (Right val, newVal, True) -> (Right val, newVal, True)


-- receives an accumulator for the result, a condition, a program to be
-- evaluated, a list variable and an assignment;
-- simulates the initial step of a for: evaluate the condition: if any error has
-- occured then pass it further on; ontherwise check the result: if it is False then 
-- the values received as an accumulator; otherwise evaluate the program and pass
-- on any errors or returns; if none has occured, then call the auxFor2 function
-- which simulates the rest of the for and return the values it computes
auxFor1 :: Res-> Expr -> Prog -> Values -> Asgn -> Res
auxFor1 acc cond prog values as2 = case (evalExprBool cond values) of
                                  Left msg -> (Left msg, values, True)
                                  Right val -> case val of
                                                True -> case (evalAdtaux prog values) of
                                                        (Left msg, _, f) -> (Left msg, values, True)
                                                        (Right val2, newVal, False) -> auxFor2 (Right val2, newVal, False) cond prog newVal as2
                                                        (Right val2, newVal, True) -> (Right val2, newVal, True)
                                                False -> acc  

-- receives a For prog and a variable list and evaluates the program in the following
-- manner: 
-- 1) evaluate the initial assign and pass on any return or error encountered
-- 2) if none of the above happened, then call the auxFor1 function to evaluate
-- the condition and execute the program inside the for body, if it is true, for one step
-- 3) the auxFor1 function calls the auxFor2 function which makes the second assignment,
-- rechecks the condition and keeps evaluating the program inside the for body until
-- the condition is no longer true: when this happens, it return the value computed for
-- the last assignment which respected the condiion. It also does one more assignment in order
-- for the counter variable used to have the right value when exiting the for loop 
evalFor :: Prog -> Values -> Res
evalFor (For as1 expr as2 prog) values = case (evalAsgn as1 values) of
                                          (Left msg, _, f) -> (Left msg, values, True)
                                          (Right val, newVal, True) -> (Right val, newVal, True)
                                          (Right val, newVal, False) -> auxFor1 (Right val, values, False) expr prog newVal as2 
 
-- receives an Assert program and a variable list and evaluates the Assert program
-- in the following manner: evaluate the Boolean expression received by the Assert
-- program and pass on any errors whichmight have occured; otherwise check the result:
-- if it is False, then pass on the message: "Assert failed"; otherwise the
-- value computed does not interest us further on, so we reutrn an arbitrary value as
-- the Int part of the result (in this case, a 1)
evalAssert :: Prog -> Values -> Res
evalAssert (Assert expr) values = case (evalExprBool expr values) of
                                    Left msg -> (Left msg, values, True)
                                    Right val -> case val of
                                                  False -> (Left "Assert failed", values, True)
                                                  True -> (Right 1, values, False)

-- receives a prog and a variable list and evaluates the prog by calling
-- the aprropriate evaluation function depending on the type of prog received;                                 
evalAdtaux :: Prog -> Values -> Res
evalAdtaux prog values = case prog of
               Eq asgn -> evalEq prog values
               Seq a b -> evalSeq prog values
               If cond p1 p2 -> evalIf prog values
               For as1 expr as2 progfor -> evalFor prog values
               Assert expr -> evalAssert prog values 
               Return expr -> ((evalExpr expr values), values, True)

-- return the first element of a Res type, namely an Either Strin Int
myFst :: Res -> Either String Int
myFst (a,b,c) = a

-- call the evalAdtAux function for the prog received as parameter and check
-- its result: if an error has occured, then return it;
-- if no return has been found (Bool value in the result of evalAdtaux is false)
-- then reutrn a "Missing reutrn" error;
-- otherwise return the first element of the reuslt, namely the computed final
-- value of the prog
evalAdt :: Prog -> Either String Int
evalAdt prog = case (evalAdtaux prog []) of
                (Left msg, vals, end) -> (Left msg)
                (Right val, vals, True) -> Right val
                (Right val, vals, False) -> Left "Missing return"

-- The *evalRaw* function is already implemented, but it relies on the *parse*
-- function which you have to implement.
--
-- Of couse, you can change this definition.  Only its name and type are
-- important.

evalRaw :: String -> Either String Int
evalRaw rawProg =
    case parse rawProg of
        Just prog -> evalAdt prog
        Nothing   -> Left "Syntax error"
