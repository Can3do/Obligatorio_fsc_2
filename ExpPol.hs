-- Juan Pablo Canedo (349963)
-- Franco Cardozo (345912)
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module ExpPol where

import Polinomios

-- ========================
-- EXPRESIONES POLINOMICAS
-- ========================

data ExPol where 
		Pol  :: Polinomio -> ExPol 
		Der  :: ExPol -> ExPol 
		Eval :: ExPol -> Int -> ExPol 
		Sum  :: [ExPol] -> ExPol 
		Prod :: [ExPol] -> ExPol 
				deriving Show

																	
--10) 
cantPol :: ExPol -> Int -- paso tests
cantPol = \expol -> case expol of {
	Pol p -> 1;
	Der e -> cantPol e;
	Eval e n -> cantPol e;
	Sum l -> case l of {
		[] -> 0;
		x:xs -> cantPol x + cantPol (Sum xs);
	};
	Prod l -> case l of {
		[] -> 0;
		x:xs -> cantPol x + cantPol (Prod xs);
	}
} 

--11)
cantx :: ExPol -> Int
cantx  = undefined

--12)
maxProd :: ExPol -> Int
maxProd  = undefined


--13)
gradoEP :: ExPol -> Int
gradoEP = undefined 
	
--14)	
calcEP :: ExPol -> Polinomio
calcEP = undefined  

--15)
resultado :: ExPol -> String
resultado = undefined

