{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Ancilla where

import Control.Category
import Prelude hiding (id,(.))
import Data.Complex
import Data.List (transpose)

import UPiBase
import UPiaBase
import UPichiaBase
import UPi hiding ((****),(++++))
import UPia hiding ((****),(++++))
import UPichia

-- this module evaluates the number of ancilla used in a UPichia program based on 3 assumptions
-- 1. for every Id operator there is only one corresponding Qubit
-- 3. the input is in quantum circuit format, i.e. in a defined universal set of gates
-- 4. the first layer should not contain measure operator

-- the module is able to output:
-- 1. evalAncilla : return the number of ancilla
-- 2. noConsecutiveMProgram : return a readable UPichia program that contains no consecutive measurement
-- 3. singleQubitMProgram : return a reabable UPichia program that expand a compound measurement (measurement on multiple Qubits) into measurements on single Qubit


-- parse UPichia into a readable format in UPi
-- parse UPi (lift R)
toUPia :: UPi (Plus a h) b -> UPi a b
toUPia (Comp (Comp g (SumC f Id)) AssocPI) = Comp (toUPia g) (toUPia f)
toUPia (Comp (Comp (Comp (ProdC f g) (Comp SwapT (Comp DistribI (SumC SwapT SwapT)))) (SumC DistribI DistribI)) AssocPI) = ProdC (toUPia f) (toUPia g)
toUPia (Comp (SumC f g) (Comp (Comp (Comp (Comp AssocPI (SumC Id AssocP)) (SumC Id (SumC SwapP Id))) (SumC Id AssocPI)) AssocP)) = SumC (toUPia f) (toUPia g)
toUPia (Comp f UnitP) = f
toUPia UnitP = id
toUPia Id = UPiBase.Inl
toUPia SwapP = UPiBase.Inr

-- parse UPia (lift L)
toUPichia :: UPi a (Times b g) -> UPi a b
toUPichia (Comp (Comp AssocT (ProdC g Id)) f) = Comp (toUPichia g) (toUPichia f)
toUPichia (Comp (Comp (Comp (Comp (Comp AssocTI (ProdC Id AssocT)) (ProdC Id (ProdC SwapT Id))) (ProdC Id AssocTI)) AssocT) (ProdC f g)) = ProdC (toUPichia f) (toUPichia g)
toUPichia (Comp UnitTI f) = f
toUPichia (Comp SwapT UnitTI) = UPiBase.Discard


-- a few constand for parsing phase in circuit format
complexY = -i
complexZ = (-1) :+ 0
complexS = i
complexT = exp(i*(pi/4))

-- parse into circuit format and measure
toCircuit :: UPi a b -> UPi a b
toCircuit SwapP = UPiBase.Px
toCircuit (Comp (SumC (Phase complexY) (Phase i)) SwapP) = UPiBase.Py
toCircuit (SumC Id (Phase i)) = UPiBase.Pz
toCircuit (SumC Id (Phase complexS)) = UPiBase.PhaseS
toCircuit (SumC Id (Phase complexT)) = UPiBase.PhaseT
toCircuit (Comp (Comp (Comp (Comp (Comp (Comp SwapT DistribI) (SumC UnitTI UnitTI)) (SumC Id SwapP)) (SumC UnitT UnitT)) Distrib) SwapT) = UPiBase.Cnot
toCircuit (Comp (Comp (Comp (Comp (Comp (Comp SwapT DistribI) (SumC UnitTI UnitTI)) (SumC Id (Comp (Comp (Comp (Comp (Comp (Comp SwapT DistribI) (SumC UnitTI UnitTI)) (SumC Id SwapP)) (SumC UnitT UnitT)) Distrib) SwapT))) (SumC UnitT UnitT)) Distrib) SwapT) = UPiBase.Toffoli
toCircuit (Comp (Comp UnitT (ProdC Id Discard)) f) = UPiBase.Measure
toCircuit (Comp g f) = Comp (toCircuit g) (toCircuit f)
toCircuit (ProdC f g) = ProdC (toCircuit f) (toCircuit g)
toCircuit a = a

-- peel FromUPi
toReadable :: UPia a (Times b g) -> UPi a b
toReadable (FromUPi a) = toCircuit (toUPichia (toUPia a))

-- peel FromUPia
toReadable' :: UPichia a b -> UPi a b
toReadable' (FromUPia a) = toReadable a


-- fix
testS :: UPichia a b -> UPi a b
testS (FromUPia (FromUPi x)) = toUPichia (toUPia x)


-- In the following section, I defined two new data type READ and R
-- READ is a readable UPichia format 
-- R is used to track the parens change from AssocT and AssocTI and therefore can be used to match any variable length of measurement
-- In this way, I can turn a UPichia program into a matrix and makes any post evaluation on measurement and ancilla very clear and easy

-- data READ captures a UPi program into a readable and comparable format
data READ = ID | H | X | Y | Z | S | T | CX | TO | M | ASSOCT | ASSOCTI | PROD READ READ | COMP READ READ deriving (Show,Eq)

-- parse UPi into READ
toRead :: UPi a b -> READ
toRead (Comp a b) = COMP (toRead a) (toRead b)
toRead (ProdC a b) = PROD (toRead a) (toRead b)
toRead Id = ID
toRead Hadamard = H
toRead Px = X
toRead Py = Y
toRead Pz = Z
toRead PhaseS = S
toRead PhaseT = T
toRead Cnot = CX
toRead Toffoli = TO
toRead Measure = M
toRead AssocT = ASSOCT
toRead AssocTI = ASSOCTI

-- peel the first layer
firstLayer :: READ -> READ
firstLayer (COMP a b) = b
firstLayer x = x

-- has AssocT or AssocTI in the layer or not
hasASSOCT :: READ -> Bool
hasASSOCT (PROD a b) = hasASSOCT a || hasASSOCT b
hasASSOCT ASSOCT = True
hasASSOCT ASSOCTI = True
hasASSOCT _ = False 

-- data R represents the number of qubits used
data R = IntVal Int | Prod R R deriving Show

assocT :: R -> R
assocT (Prod (Prod a b) c) = Prod a (Prod b c)

assocTI :: R -> R
assocTI (Prod a (Prod b c)) = Prod (Prod a b) c

-- change the parens of R according to AssocT and AssocTI
changeParens:: READ -> R -> R
changeParens (PROD a b) (Prod x y) = Prod (changeParens a x) (changeParens b y)
changeParens ID x = x
changeParens ASSOCT x = assocT x
changeParens ASSOCTI x = assocTI x

-- parse READ into R
toR :: READ -> R
toR (PROD a b) = Prod (toR a) (toR b)
toR ID = IntVal 1
toR H = IntVal 1
toR X = IntVal 1
toR Y = IntVal 1
toR Z = IntVal 1
toR S = IntVal 1
toR T = IntVal 1
toR CX = Prod (IntVal 1) (IntVal 1)
toR TO = Prod (IntVal 1) (Prod (IntVal 1) (IntVal 1))

-- calculate the number of qubits used
calQUBIT :: R -> Int
calQUBIT (Prod a b) = calQUBIT a + calQUBIT b
calQUBIT (IntVal a) = a

-- find for every layer the length of stretchable measurements, length by the number of Qubits
multiMeasure :: READ -> R -> [[Int]]
multiMeasure (COMP a b) x = 
    if hasASSOCT b 
        then multiMeasure a (changeParens b x) 
        else multiMeasure a x ++ multiMeasure b x
multiMeasure (PROD a b) (Prod c d) = [concat (multiMeasure a c ++ multiMeasure b d)]
multiMeasure M x = [[calQUBIT x]]
multiMeasure _ _ = [[]]

-- parse READ into a 2-D matrix of READ
toRow :: READ -> [[READ]]
toRow (COMP a b) = 
    if hasASSOCT b
        then toRow a
        else toRow a ++ toRow b
toRow (PROD a b) = [concat (toRow a ++ toRow b)]
toRow ID = [[ID]]
toRow H = [[H]]
toRow X = [[X]]
toRow Y = [[Y]]
toRow Z = [[Z]]
toRow S = [[S]]
toRow T = [[T]]
toRow CX = [[CX,CX]]
toRow TO = [[TO,TO,TO]]
toRow M = [[M]]

-- helper function for singleLayerMeasure
-- replicate an element of READ n times
replicateREAD :: Int -> READ -> [READ]
replicateREAD n a
    | n <= 0    = []
    | otherwise = a:replicateREAD (n-1) a

-- expand a stretchable measurement into a number of single Qubit measurement in one layer
singleLayerMeasure :: [READ] -> [Int] -> [READ]
singleLayerMeasure [] _ = []
singleLayerMeasure x [] = x
singleLayerMeasure (x:xs) (y:ys)
    | x == M    = replicateREAD y M ++ singleLayerMeasure xs ys
    | otherwise = x : singleLayerMeasure xs (y:ys)


-- expand stretchable measurements for all layers
multiLayerMeasure :: [[READ]] -> [[Int]] -> [[READ]]
multiLayerMeasure [[]] _ = [[]]
multiLayerMeasure x [[]] = x
multiLayerMeasure (x:xs) (y:ys) = singleLayerMeasure x y : multiLayerMeasure xs ys


-- parse UPi into a matrix of readable format
toMatrix :: UPi a b -> [[READ]]
toMatrix a = multiLayerMeasure (toRow (toRead a)) (multiMeasure (toRead a) (toR (firstLayer (toRead a))))

-- in the following, I take the transpose of matrix and operate on columns, where consecutive measurements are exposed easily

-- parse consecutive measurements in a single column
singleColumn :: [READ] -> [READ]
singleColumn [] = []
singleColumn (M:x@M:xs) = ID : singleColumn (x:xs)
singleColumn (M:xs@(ID:_)) = 
    if head (dropWhile isID xs) == M
        then ID : takeWhile isID xs ++ singleColumn (dropWhile isID xs)
        else M : singleColumn xs
    where isID x = x == ID
singleColumn (x:xs) = x : singleColumn xs

-- parse consecutive measurements in the matrix
-- the input should be the transpose of the toMatrix function
multiColumns :: [[READ]] -> [[READ]]
multiColumns = map singleColumn

-- calculate number of M in a column
countM :: [READ] -> Int
countM x = length (filter (== M) x)

-- calculate the total number of M in the matrix
countM' :: [[READ]] -> Int
countM' x = sum (map countM x) 

-- number of ancilla
evalAncilla :: UPichia a b -> Int
evalAncilla x = countM' (multiColumns (transpose (toMatrix (toReadable' x))))

-- return a readable UPichia program that contains no consecutive measurement
noConsecutiveMProgram :: UPichia a b -> [[READ]]
noConsecutiveMProgram x = reverse (transpose  (multiColumns (transpose (toMatrix (toReadable' x)))))

-- return a reabable UPichia program that expand a compound measurement (measurement on multiple Qubits) into measurements on single Qubit
singleQubitMProgram :: UPichia a b -> [[READ]]
singleQubitMProgram x = reverse (toMatrix (toReadable' x))



-- A slightly optimzed version of UPia.arr' . UPi.arr'.
gate :: UPi a b -> UPichia a b
gate f = FromUPia (FromUPi (UPi.unitp >>> f >>> UPi.unitti))

-- test examples
-- test passed
ex1 = (id **** gate UPi.h **** id) >>> 
      (gate UPi.cnot **** id) >>> 
      (id **** gate UPi.h **** id) >>>
      UPichia.assoct >>>
      (id **** gate UPi.cnot) >>> 
      UPichia.assocti >>>
      (id **** id **** gate UPi.pz) >>> 
      (id **** measure **** measure)
ex2 = gate UPi.h >>> measure
ex3 = gate UPi.h **** gate UPi.h >>> measure
ex4 = gate UPi.h **** gate UPi.h >>> id **** measure >>> measure
ex5 = gate UPi.h **** gate UPi.h >>> measure >>> measure
ex6 = gate UPi.h **** gate UPi.h >>> id **** measure >>> id **** measure >>> id **** measure
ex7 = gate UPi.h **** gate UPi.h **** gate UPi.h >>> measure **** id >>> UPichia.assoct >>> id **** measure
ex8 = gate UPi.h **** gate UPi.h **** gate UPi.h >>> id **** id **** measure >>> gate UPi.h **** measure **** gate UPi.h >>> measure
ex9 = gate UPi.h **** gate UPi.h >>> measure >>> measure >>> measure
ex10 = gate UPi.h **** gate UPi.cnot **** gate UPi.h >>> gate UPi.h **** measure **** gate UPi.h >>> measure **** gate UPi.h
ex11 = gate UPi.h **** gate UPi.cnot **** gate UPi.h >>> id **** (id **** id) **** measure >>> gate UPi.h **** measure **** gate UPi.h
ex12 = gate UPi.h **** gate UPi.h **** gate UPi.h >>> id **** id **** measure >>> id **** measure **** id >>> measure
ex13 = gate UPi.h **** (gate UPi.h **** gate UPi.h) **** gate UPi.h >>> gate UPi.h **** measure **** gate UPi.h
ex14 = gate UPi.h **** gate UPi.h **** gate UPi.h **** (gate UPi.h **** gate UPi.h) >>> gate UPi.h **** measure **** gate UPi.h **** measure
ex15 = gate UPi.h **** gate UPi.h **** gate UPi.h **** gate UPi.h >>>
       gate UPi.cnot **** measure **** measure >>>
       UPichia.assoct **** id >>>
       measure
ex16 = gate UPi.h UPichia.**** gate UPi.h **** gate UPi.h >>>
        measure **** id **** measure >>>
        measure
ex17 = gate UPi.h **** (gate UPi.h **** gate UPi.h) **** gate UPi.h **** (gate UPi.h **** gate UPi.h) >>> id **** measure **** id **** measure
ex18 = gate UPi.h **** (gate UPi.cnot **** gate UPi.h) **** gate UPi.toffoli >>>
       id **** measure **** measure >>>
       UPichia.assocti **** id >>>
       measure **** measure **** (id **** (id **** id))
ex19 = gate UPi.h **** gate UPi.cnot **** (gate UPi.h **** gate UPi.toffoli) >>>
       id **** measure **** (id **** measure) >>>
       measure **** (id **** id) **** (measure **** (id **** (measure **** id)))
ex20 = gate UPi.px **** gate UPi.py **** gate UPi.toffoli >>>
       gate UPi.cnot **** (measure **** gate UPi.cnot) >>>
       measure
-- test failed
ex21 = gate UPi.px **** gate UPi.py **** gate UPi.pz **** gate UPi.s **** gate UPi.t 
