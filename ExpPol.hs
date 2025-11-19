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
cantx :: ExPol -> Int -- paso tests
cantx  = \expol -> case expol of {
	Pol p -> case p of {
		[] -> 0;
		x:xs -> case x of {
			(c,e) -> case (e > 0) of {
				True -> 1 + cantx (Pol xs);
				False -> cantx (Pol xs);
			}
		}
	};
	Der e -> cantx e;
	Eval e n -> cantx e;
	Sum l -> case l of {
		[] -> 0;
		x:xs -> cantx x + cantx (Sum xs);
	};
	Prod l -> case l of {
		[] -> 0;
		x:xs -> cantx x + cantx (Prod xs);
	}
} 

--12)
maxProd :: ExPol -> Int -- paso tests
maxProd = \expol -> case expol of {
	Pol p -> 0;
	Der e -> maxProd e;
	Eval e n -> maxProd e;
	Sum l -> case l of {
		[] -> 0;
		x:xs -> case (maxProd x > maxProd (Sum xs)) of {
			True -> maxProd x;
			False -> maxProd (Sum xs);
		}
	};
	Prod l -> case l of {
		[] -> 0;
		x:xs -> case (length l > maxProd (Sum l)) of { -- este fue un trucazo la verdad, uso la suma de arriba para checkear el maxProd de cada elemento interno de la multiplicacion porque no pasaba el test 7
			True -> length l;
			False -> maxProd (Sum l);
		}
	};
} 


--13)
gradoEP :: ExPol -> Int -- pasó tests
gradoEP = \expol -> case expol of {
	Pol p -> gradoPol p;
	Der e -> gradoEP e;
	Eval e n -> gradoEP e;
	Sum l -> case l of {
		[] -> 0;
		x:xs -> case (gradoEP x > gradoEP (Sum xs)) of {
			True -> gradoEP x;
			False -> gradoEP (Sum xs);
		}
	};
	Prod l -> case l of {
		[] -> 0;
		x:xs -> case (gradoEP x > gradoEP (Prod xs)) of {
			True -> gradoEP x;
			False -> gradoEP (Prod xs);
		}
	}
} 
	
--14)	
calcEP :: ExPol -> Polinomio -- paso test
calcEP = \expol -> case expol of {
	Pol p -> p;
	Der e -> derPol (calcEP e);
	Eval e n -> [(evalPol (calcEP e) n,0)];
	Sum l -> case l of {
		[] -> [];
		x:xs -> sumPol (calcEP x) (calcEP (Sum xs));
	};
	Prod l -> case l of {
		[] -> [(1,0)]; -- 1 es el neutro de la multiplicacion, por eso no pasaba tests
		x:xs -> mulPol (calcEP x) (calcEP (Prod xs));
	};
}  

--15)
resultado :: ExPol -> String
resultado = \expol -> showPol (calcEP expol);

