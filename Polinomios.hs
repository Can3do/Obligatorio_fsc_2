-- Juan Pablo Canedo (349963)
-- Franco Cardozo (345912)
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Polinomios where

type Monomio = (Int, Int)
type Polinomio = [Monomio]

-- ======================
-- POLINOMIOS
-- ======================

--1)
agregarMon :: Monomio -> Polinomio -> Polinomio -- pa que horrible quedo esto pero ta, funciona - pasó tests
agregarMon = \mon -> \pol -> case mon of {
    (c, e) -> case c of {
        0 -> pol;
        n -> case pol of {
            [] -> mon:pol;
            x:xs -> case x of {
                (d, f) -> case f == e of {
                    True -> case c+d == 0 of {
                        True -> xs;
                        False -> (c+d,e):xs;
                    };
                    False -> case e > f of {
                        True -> mon:pol;
                        False -> x:agregarMon mon xs;
                    }
                }
            }
        }
    }    
}

--2)
redPol :: Polinomio -> Polinomio -- pasó tests
redPol = \pol -> case pol of {
    [] -> [];
    x:xs -> case x of {
        (0, e) -> redPol xs;
        (c, e) -> agregarMon x (redPol xs);
    }
}	

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio -- pasó tests
sumPol = \pol1 -> \pol2 -> case pol1 of {
    [] -> pol2;
    x:xs -> case pol2 of {
        [] -> pol1;
        y:ys -> sumPol (agregarMon y pol1) ys; -- acumulo todo en el primer polinomio
    }
}

--4)
mulPol :: Polinomio -> Polinomio -> Polinomio -- TODOooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
mulPol = undefined

--5)
derPol :: Polinomio -> Polinomio -- pasó tests
derPol = \pol -> case pol of {
    [] -> [];
    x:xs -> case x of {
        (c,e) -> agregarMon (c*e,e-1) (derPol xs);
    }
}

--6)
evalPol :: Polinomio -> Int -> Int -- Pasó tests
evalPol = \pol -> \n -> case pol of {
    [] -> 0;
    x:xs -> case x of {
        (c,e) -> c*(n^e) + evalPol xs n;
    }
}

--7)
gradoPol::Polinomio -> Int -- paso tests
gradoPol = \pol -> case pol of {
    [] -> 0;
    x:xs -> case x of {
        (0, e) -> gradoPol xs;
        (c, e) -> max e (gradoPol xs);
    }
}
																	
																	
-- ======================
-- SHOW
-- ======================

--8)
showMon :: Monomio -> String -- paso tests
showMon = \mon -> case mon of {
    (c, e) -> case c of {
        0 -> "";
        1 -> case e of {
            0 -> show c;
            1 -> "x";
            m -> "x" ++ "^" ++ show e;
        };
        -1 -> case e of {
            0 -> show c;
            1 -> "-x";
            m -> "-x" ++ "^" ++ show e;
        };
        n -> case e of {
            0 -> show c;
            1 -> show c ++ "x";
            m -> show c ++ "x" ++ "^" ++ show e;
        }
    }
}

--9)
showPol :: Polinomio -> String -- Pasó test
showPol = \pol -> case pol of {
    [] -> "";
    x:xs -> case xs of {
        [] -> showMon x;
        y:ys -> case y of {
            (c, e) -> case c < 0 of {
                True -> showMon x ++ "" ++ showPol xs;
                False -> showMon x ++ "+" ++ showPol xs;
            }
        }
    }
}  
