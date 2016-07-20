-- places in the net
type Mark = ([String],[String],[String]
            ,[String],[String],[String]
			,[String],[String])
			
-- marking at intial state
m0 :: Mark 
m0 = (["CardInsert"],[],[],[],[],[]
     ,[],[])

type Name = String
type Condition = Mark -> Bool
type Action = Mark -> Mark
type Transition = (Name, Condition, Action)

-- behaviour transition action
ts :: [Transition]
ts = [("T1", \(p1,p2,p3,p4,p5,p6,p7,p8)->p1/= []
     , \((x:xs),p2,p3,p4,p5,p6,p7,p8)->(xs,"EnterPin":p2,p3,p4,p5,p6,p7,p8))
	 
     ,("T2", \(p1,p2,p3,p4,p5,p6,p7,p8)->p2/= []
	 , \(p1,(x:xs),p3,p4,p5,p6,p7,p8)->(p1,xs,"BankVerify":p3,p4,p5,p6,p7,p8))
	 
	 ,("T3", \(p1,p2,p3,p4,p5,p6,p7,p8)->p3/= []
	 , \(p1,p2,(x:xs),p4,p5,p6,p7,p8)->(p1,p2,xs,"ClickWithdrawal":p4,p5,p6,p7,p8))
	 
	 ,("T4", \(p1,p2,p3,p4,p5,p6,p7,p8)->p3/= [] 
	 ,\(p1,p2,(x:xs),p4,p5,p6,p7,p8)->(p1,p2,xs,p4,p5,p6,p7,"CardEject":p8))
	 
	 ,("T5", \(p1,p2,p3,p4,p5,p6,p7,p8)->p4/= []
	 , \(p1,p2,p3,(x:xs),p5,p6,p7,p8)->(p1,p2,p3,xs,"EnterAmount":p5,p6,p7,p8))  
	 
	 ,("T6", \(p1,p2,p3,p4,p5,p6,p7,p8)->p5/= []
	 , \(p1,p2,p3,p4,(x:xs),p6,p7,p8)->(p1,p2,p3,p4,xs,"Transaction":p6,p7,p8))
	 
	 ,("T7", \(p1,p2,p3,p4,p5,p6,p7,p8)->p5/= []
	 , \(p1,p2,p3,p4,(x:xs),p6,p7,p8)->(p1,p2,p3,p4,xs,p6,p7,"CardEject":p8))
	 
	 ,("T8", \(p1,p2,p3,p4,p5,p6,p7,p8)->p6/= []
	 , \(p1,p2,p3,p4,p5,(x:xs),p7,p8)->(p1,p2,p3,p4,p5,xs,"PrintReceipt":p7,p8))
	 
	 ,("T9", \(p1,p2,p3,p4,p5,p6,p7,p8)->p7/= []
	 , \(p1,p2,p3,p4,p5,p6,(x:xs),p8)->(p1,p2,p3,p4,p5,p6,xs,"CardEject":p8))]

-- to print transition
printTs [] = "" -- no transition to print
-- n = name, c = conditions, a = action
printTs ((n,c,a):ts) = n ++ (printTs ts)
-- (printTs ts) will print the other transition in the net

-- to enable the transition
enabledTs [] m = [] -- m is the net
enabledTs ((n,c,a):ts) m = if (c m) 
                           then (n,c,a):(enabledTs ts m) 
-- can be defined as a list of the current transition that 
-- is available and the remaining transitions in the net 
   						   else (enabledTs ts m)
-- (enabledTs ts m) will check for the other
-- transitons in the net that may be aviailble
						   
-- applying the transition
applyTs [] m name = m 
-- name is transition name that will be given by the user  
applyTs ((n,c,a):ts) m name = if (n==name) 
                              then (a m) 
							  else (applyTs ts m name)
-- (applyTs ts m name) will check for the other 
-- transition that may be the same name as given by
-- the user and then apply the action on the net. 

run m0 = do
    putStrLn "\n"
    putStr "Printing the marking on places \n"
    print m0
    putStr "Printing the Available Transitions \n"
    putStrLn (printTs (enabledTs ts m0))
    putStr "Input the Transition \n"
    t <- getLine -- getting the input from user
    putStr "Applying the Transition \n"
    run (applyTs ts m0 t) -- apply the above 
-- input transition on the marking and then go back
-- for next transition available. 
	
main = do -- from here the execution of program starts 
    run m0 -- intial marking
 
 