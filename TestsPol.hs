module TestsPol where

-- Requisitos: módulos Polinomios y ExpPol con las funciones pedidas
import Polinomios
import ExpPol

-- =============================================================
-- Polinomios reducidos para usar en pruebas
-- =============================================================
p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 :: Polinomio
p1  = [(2,3),(1,1),(-5,0)]                    --  2x^3 + x - 5
p2  = [(3,4),(-2,3),(4,1)]                    --  3x^4 - 2x^3 + 4x
p3  = [(5,0)]                                  --  5
p4  = [(2,3)]                                  --  2x^3
p5  = []                                       --  0
p6  = [(1,6),(-4,2),(2,0)]                    --  x^6 - 4x^2 + 2
p7  = [(3,1)]                                  --  3x
p8  = [(-7,4)]                                 -- -7x^4
p9  = [(1,1),(1,0)]                            --  x + 1
p10 = [(-2,2),(5,0)]                           -- -2x^2 + 5

-- =============================================================
-- 1) agregarMon :: Monomio -> Polinomio -> Polinomio  (ag1..ag10)
-- =============================================================
ag1 = agregarMon (2,5) p1   == [(2,5)] ++ p1                         -- inserta arriba de todo
ag2 = agregarMon (3,1) p1   == [(2,3),(4,1),(-5,0)]                  -- combina con epp=1
ag3 = agregarMon (-1,1) p1  == [(2,3),(-5,0)]                        -- x + (-x) = 0
ag4 = agregarMon (0,7) p2   == p2                                    -- coef 0 no altera
ag5 = agregarMon (-4,3) p2  == [(3,4),(-6,3),(4,1)]                  -- combina negativo
ag6 = agregarMon (1,0) p4   == [(2,3),(1,0)]                         -- agrega constante
ag7 = agregarMon (-5,0) p3  == []                                    -- polinomio vacío
ag8 = agregarMon (1,6) p6   == [(2,6),(-4,2),(2,0)]                  -- combina tope
ag9 = agregarMon (-4,2) p6  == [(1,6),(-8,2),(2,0)]                  -- refuerza combinación intermedia
ag10 = agregarMon (7,4) p5   == [(7,4)]                              -- agrega a vacío

tests_agregarMon = [ag1,ag2,ag3,ag4,ag5,ag6,ag7,ag8,ag9,ag10]

-- =============================================================
-- 2) redPol :: Polinomio -> Polinomio   (rp1..rp10)
-- =============================================================
rp1  = redPol [(-2,1),(0,1),(3,2)] == [(3,2),(-2,1)]
rp2  = redPol [(1,4),(-1,1),(0,2),(3,2),(-1,4),(-1,1)] == [(3,2),(-2,1)]
rp3  = redPol [(-5,1),(0,2),(1,2),(0,1),(3,1),(2,2)] == [(3,2),(-2,1)]
rp4  = redPol [(2,2),(2,2),(-4,2)] == []                       -- todo se cancela
rp5  = redPol [(0,5),(0,0)] == []                              -- solo ceros
rp6  = redPol [(3,2),(-3,2),(5,0),(-5,0)] == []                -- constante + cuadrático se anulan
rp7  = redPol [(4,6),(3,2),(0,6),(-3,2),(1,0)] == [(4,6),(1,0)]
rp8  = redPol [(0,3),(2,1),(0,1),(5,0)] == [(2,1),(5,0)]
rp9  = redPol [(1,8),(1,8),(2,8),(-3,8)] == [(1,8)]             
rp10 = redPol [(5,3),(-5,3),(1,1),(-1,1),(7,0)] == [(7,0)]     -- solo queda la constante

tests_redPol = [rp1,rp2,rp3,rp4,rp5,rp6,rp7,rp8,rp9,rp10]

-- =============================================================
-- 3) sumPol :: Polinomio -> Polinomio -> Polinomio  (su1..su10)
-- =============================================================
su1  = sumPol p1 p5 == p1                                     -- + 0
su2  = sumPol p5 p2 == p2
su3  = sumPol p1 p4 == [(4,3),(1,1),(-5,0)]                   -- combina epp=3
su4  = sumPol p3 p3 == [(10,0)]                               -- 5 + 5
su5  = sumPol p8 [(7,4)] == []                                -- -7x^4 + 7x^4 = 0
su6  = sumPol p2 p10 == [(3,4),(-2,3),(-2,2),(4,1),(5,0)]     -- mezcla diversa
su7  = sumPol p9 p7 == [(4,1),(1,0)]
su8  = sumPol p6 [(-1,6),(4,2),(-2,0)] == []                  -- suma con su opuesto
su9  = sumPol p1 [(-2,3),(-1,1),(5,0)] == []                  -- opuesto epacto
su10 = sumPol p2 [(-3,4),(2,3),(-4,1)] == []                  -- opuesto epacto

tests_sumPol = [su1,su2,su3,su4,su5,su6,su7,su8,su9,su10]

-- =============================================================
-- 4) mulPol :: Polinomio -> Polinomio -> Polinomio  (mu1..mu10)
-- =============================================================
mu1  = mulPol p5 p1 == []                                       -- 0 * p = 0
mu2  = mulPol p1 p5 == []
mu3  = mulPol p3 p1 == [(10,3),(5,1),(-25,0)]                   -- 5 * p1
mu4  = mulPol p4 p2 == [(6,7),(-4,6),(8,4)]                     -- (2x^3)*p2
mu5  = mulPol p9 p9 == [(1,2),(2,1),(1,0)]                      -- (x+1)^2
mu6  = mulPol p7 p7 == [(9,2)]                                  -- (3x)^2
mu7  = mulPol p8 [(1,4)] == [(-7,8)]                            -- (-7x^4)*x^4
mu8  = mulPol p6 p10 == [(-2,8),(5,6),(8,4),(-24,2),(10,0)]
mu9  = mulPol [(1,2),(-1,0)] [(1,2),(-1,0)] == [(1,4),(-2,2),(1,0)]  -- (x^2-1)^2
mu10 = mulPol p2 [(-3,4),(2,3),(-4,1)] == [(-9,8),(12,7),(-4,6),(-24,5),(16,4),(-16,2)]                        

tests_mulPol = [mu1,mu2,mu3,mu4,mu5,mu6,mu7,mu8,mu9,mu10]

-- =============================================================
-- 5) derPol :: Polinomio -> Polinomio  (de1..de10)
-- =============================================================
de1  = derPol p5 == []                                            -- d(0) = 0
de2  = derPol p3 == []                                            -- d(5) = 0
de3  = derPol p7 == [(3,0)]                                       -- d(3x) = 3
de4  = derPol p4 == [(6,2)]                                       -- d(2x^3) = 6x^2
de5  = derPol p2 == [(12,3),(-6,2),(4,0)]                         -- mezcla
de6  = derPol p6 == [(6,5),(-8,1)]                                -- d(x^6 - 4x^2 + 2)
de7  = derPol p8 == [(-28,3)]                                     -- d(-7x^4) = -28x^3
de8  = derPol p9 == [(1,0)]                                       -- d(x+1) = 1
de9  = derPol p10 == [(-4,1)]                                     -- d(-2x^2+5) = -4x
de10 = derPol [(0,5)] == []                                        -- término nulo desaparece

tests_derPol = [de1,de2,de3,de4,de5,de6,de7,de8,de9,de10]

-- =============================================================
-- 6) evalPol :: Polinomio -> Int -> Int  (ev1..ev10)
-- =============================================================
ev1  = evalPol p5 100  == 0
ev2  = evalPol p3 0    == 5
ev3  = evalPol p9 1    == 2                  
ev4  = evalPol p9 (-1) == 0                 
ev5  = evalPol p1 2    == 13
ev6  = evalPol p2 0    == 0
ev7  = evalPol p6 1    == -1
ev8  = evalPol p6 1    == -1
ev9  = evalPol p8 2    == -112         
ev10 = evalPol p10 (-2) == -3

tests_evalPol = [ev1,ev2,ev3,ev4,ev5,ev6,ev7,ev8,ev9,ev10]

-- =============================================================
-- 7) gradoPol :: Polinomio -> Int   (gr1..gr10)
-- =============================================================
gr1  = gradoPol p5  == 0
gr2  = gradoPol p3  == 0
gr3  = gradoPol p1  == 3
gr4  = gradoPol p2  == 4
gr5  = gradoPol p4  == 3
gr6  = gradoPol p6  == 6
gr7  = gradoPol p7  == 1
gr8  = gradoPol p8  == 4
gr9  = gradoPol p9  == 1
gr10 = gradoPol p10 == 2

tests_gradoPol = [gr1,gr2,gr3,gr4,gr5,gr6,gr7,gr8,gr9,gr10]

-- =============================================================
-- 8) showMon :: Monomio -> String   (sm1..sm10)
-- =============================================================
sm1  = showMon (0,3)  == ""
sm2  = showMon (5,0)  == "5"
sm3  = showMon (-7,0) == "-7"
sm4  = showMon (1,1)  == "x"
sm5  = showMon (-1,1) == "-x"
sm6  = showMon (1,5)  == "x^5"
sm7  = showMon (-1,5) == "-x^5"
sm8  = showMon (3,2)  == "3x^2"
sm9  = showMon (-7,4) == "-7x^4"
sm10 = showMon (2,1)  == "2x"

tests_showMon = [sm1,sm2,sm3,sm4,sm5,sm6,sm7,sm8,sm9,sm10]

-- =============================================================
-- 9) showPol :: Polinomio -> String   (sp1..sp10)
-- =============================================================
sp1  = showPol []      == ""
sp2  = filter (/=' ') (showPol p3) == "5"
sp3  = filter (/=' ') (showPol p7) == "3x"
sp4  = filter (/=' ') (showPol p9) == "x+1"
sp5  = filter (/=' ') (showPol p1) == "2x^3+x-5"
sp6  = filter (/=' ') (showPol p2) == "3x^4-2x^3+4x"
sp7  = filter (/=' ') (showPol p6) == "x^6-4x^2+2"
sp8  = filter (/=' ') (showPol p8) == "-7x^4"
sp9  = filter (/=' ') (showPol p10) == "-2x^2+5"
sp10 = filter (/=' ') (showPol [(1,4),(3,2),(-2,1),(7,0)]) == "x^4+3x^2-2x+7"

tests_showPol = [sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10]

-- =============================================================
-- Expresiones polinómicas para usar en pruebas
-- =============================================================
ep1, ep2, ep3, ep4, ep5, ep6, ep7, ep8, ep9, ep10 :: ExPol

ep1 = Sum [ Prod [ Eval (Pol [(1,2)]) 2 , Der (Pol [(2,6),(1,0)]) ] , Pol [(1,4)] ] 
ep2 = Sum [ Sum [ Prod [ Pol [(1,2),(1,1)] , Pol [(1,3)] ] , Der (Pol [(1,4)]) ] , Eval (Pol [(2,5),(3,4)]) 5 ] 
ep3 = Der (Prod[Pol [(8,0)]]) 
ep4 = Prod [ Pol [(1,1),(5,0)] , Pol [(4,0),(-2,1)] ] 
ep5 = Eval (Pol [(4,3),(7,2),(3,1),(5,0)]) 4 
ep6 = Sum [ Prod [ Der (Pol [(1,2),(2,0)]) , Der (Pol [(3,1),(5,0)]) ] , Prod [ Eval (Pol [(1,4),(2,0)]) 2 , Eval (Pol [(1,1)]) 0 ] ] 
ep7 = Prod [ Prod [ Prod [ Pol [(2,0)] , Pol [(1,1),(1,0)] , Eval (Pol [(5,6),(27,0)]) 2] , Pol [(1,1),(-2,0)] ] , Pol [(1,1),(5,0)] ] 
ep8 = Sum [ Sum [ Pol [(1,2),(-2,1)] , Der (Pol [(1,4)]) ] , Eval (Pol [(2,10),(-8,0)]) 2 ] 
ep9 = Prod [ep1,ep2,ep4,ep5,ep6,ep7] 
ep10 = Sum [Prod[ep6,ep7],Prod[ep1,ep2,ep3,ep4,ep5,ep6,ep7],ep8]

-- ============================================
-- 10) cantPol :: ExPol -> Int  (cp1..cp10)
-- ============================================
cp1  = cantPol ep1  == 3
cp2  = cantPol ep2  == 4
cp3  = cantPol ep3  == 1
cp4  = cantPol ep4  == 2
cp5  = cantPol ep5  == 1
cp6  = cantPol ep6  == 4
cp7  = cantPol ep7  == 5
cp8  = cantPol ep8  == 3
cp9  = cantPol ep9  == 19
cp10 = cantPol ep10 == 32

tests_cantPol = [cp1,cp2,cp3,cp4,cp5,cp6,cp7,cp8,cp9,cp10]

-- ============================================
-- 11) cantx :: ExPol -> Int  (cx1..cx10)
-- ============================================cx1  = cantMas ep1  == 1
cx1  = cantx ep1  == 3
cx2  = cantx ep2  == 6
cx3  = cantx ep3  == 0
cx4  = cantx ep4  == 2
cx5  = cantx ep5  == 3
cx6  = cantx ep6  == 4
cx7  = cantx ep7  == 4
cx8  = cantx ep8  == 4
cx9  = cantx ep9  == 22
cx10 = cantx ep10 == 34

tests_cantx = [cx1,cx2,cx3,cx4,cx5,cx6,cx7,cx8,cx9,cx10]

-- ============================================
-- 12) maxProd :: ExPol -> Int  (mp1..mp10)
-- ============================================
mp1  = maxProd ep1  == 2
mp2  = maxProd ep2  == 2
mp3  = maxProd ep3  == 1
mp4  = maxProd ep4  == 2
mp5  = maxProd ep5  == 0
mp6  = maxProd ep6  == 2
mp7  = maxProd ep7  == 3
mp8  = maxProd ep8  == 0
mp9  = maxProd ep9  == 6
mp10 = maxProd ep10 == 7

tests_maxProd = [mp1,mp2,mp3,mp4,mp5,mp6,mp7,mp8,mp9,mp10]

-- ============================================
-- 13) gradoEP :: ExPol -> Int  (gep1..gep10)
-- ============================================
gep1  = gradoEP ep1  == 6
gep2  = gradoEP ep2  == 5
gep3  = gradoEP ep3  == 0
gep4  = gradoEP ep4  == 1
gep5  = gradoEP ep5  == 3
gep6  = gradoEP ep6  == 4
gep7  = gradoEP ep7  == 6
gep8  = gradoEP ep8  == 10
gep9  = gradoEP ep9  == 6
gep10 = gradoEP ep10 == 10

tests_gradoEP = [gep1,gep2,gep3,gep4,gep5,gep6,gep7,gep8,gep9,gep10]

-- ============================================
-- 14) calcEP :: ExPol -> Polinomio  (cep1..cep10)
-- ============================================
cep1  = calcEP ep1  == [(48,5),(1,4)]
cep2  = calcEP ep2  == [(1,5),(1,4),(4,3),(8125,0)]
cep3  = calcEP ep3  == []
cep4  = calcEP ep4  == [(-2,2),(-6,1),(20,0)]
cep5  = calcEP ep5  == [(385,0)]
cep6  = calcEP ep6  == [(6,1)]
cep7  = calcEP ep7  == [(694,3),(2776,2),(-4858,1),(-6940,0)]
cep8  = calcEP ep8  == [(4,3),(1,2),(-2,1),(2040,0)]
cep9  = calcEP ep9  == [(-153901440,16),(-1234417800,15),(-949058880,14),(7368031440,13),(8002874880,12)
                       ,(-1228123872360,11),(-8818748095080,10),(6007494616200,9),(88910865813000,8)
                       ,(-48168345225000,7),(-126086961000000,6),(-2605102500000,5)]
cep10 = calcEP ep10 == [(4164,4),(16660,3),(-29147,2),(-41642,1),(2040,0)]

tests_calcEP = [cep1,cep2,cep3,cep4,cep5,cep6,cep7,cep8,cep9,cep10]

-- ============================================
-- 15) resultado :: ExPol -> String  (res1..res10)
-- ============================================
res1  = filter (/=' ') (resultado ep1)  == "48x^5+x^4"
res2  = filter (/=' ') (resultado ep2)  == "x^5+x^4+4x^3+8125"
res3  = filter (/=' ') (resultado ep3)  == ""
res4  = filter (/=' ') (resultado ep4)  == "-2x^2-6x+20"
res5  = filter (/=' ') (resultado ep5)  == "385"
res6  = filter (/=' ') (resultado ep6)  == "6x"
res7  = filter (/=' ') (resultado ep7)  == "694x^3+2776x^2-4858x-6940"
res8  = filter (/=' ') (resultado ep8)  == "4x^3+x^2-2x+2040"
res9  = filter (/=' ') (resultado ep9)  == "-153901440x^16-1234417800x^15-949058880x^14+7368031440x^13+8002874880x^12-1228123872360x^11-8818748095080x^10+6007494616200x^9+88910865813000x^8-48168345225000x^7-126086961000000x^6-2605102500000x^5"
res10 = filter (/=' ') (resultado ep10) == "4164x^4+16660x^3-29147x^2-41642x+2040"

tests_resultado = [res1,res2,res3,res4,res5,res6,res7,res8,res9,res10]


-- =============================================================
-- Agregadores
-- =============================================================

test_agregarMon = and tests_agregarMon
test_redPol     = and tests_redPol
test_sumPol     = and tests_sumPol
test_mulPol     = and tests_mulPol
test_derPol     = and tests_derPol
test_evalPol    = and tests_evalPol
test_gradoPol   = and tests_gradoPol
test_showMon    = and tests_showMon
test_showPol    = and tests_showPol
test_cantPol    = and tests_cantPol
test_cantx      = and tests_cantx
test_maxProd    = and tests_maxProd
test_gradoEP    = and tests_gradoEP
test_calcEP     = and tests_calcEP
test_resultado  = and tests_resultado

testPol :: Bool
testPol = and [test_agregarMon, test_redPol, test_sumPol, test_mulPol, test_derPol, test_evalPol, test_gradoPol]

testShow :: Bool
testShow = and [test_showMon, test_showPol]

testExPol :: Bool
testExPol = and [test_cantPol, test_cantx, test_maxProd, test_gradoEP, test_calcEP, test_resultado]

testAll :: Bool
testAll = testPol && testShow && testExPol