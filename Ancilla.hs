{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Example where

import Control.Category
import Prelude hiding (id,(.))
import Data.Complex

import UPiBase
import UPiaBase
import UPichiaBase
import UPi hiding ((****),(++++))
import UPia hiding ((****),(++++))
import UPichia

-- count the number of time cloneQubit appears
evalHeap :: UPi a b -> Int
evalHeap (Comp (Comp (Comp (Comp (Comp DistribI UnitP) (SumC (Comp (SumC (Comp (Comp (Comp (ProdC Id UnitP) (Comp SwapT (Comp DistribI (SumC SwapT SwapT)))) (SumC DistribI DistribI)) AssocPI) (Comp (Comp (Comp (ProdC SwapP UnitP) (Comp SwapT (Comp DistribI (SumC SwapT SwapT)))) (SumC DistribI DistribI)) AssocPI)) (Comp (Comp (Comp (Comp AssocPI (SumC Id AssocP)) (SumC Id (SumC SwapP Id))) (SumC Id AssocPI)) AssocP)) Id)) AssocPI) (SumC (Comp (SumC (Comp UnitTI UnitP) (Comp UnitTI UnitP)) (Comp (Comp (Comp (Comp AssocPI (SumC Id AssocP)) (SumC Id (SumC SwapP Id))) (SumC Id AssocPI)) AssocP)) Id)) AssocPI) = 1
evalHeap (Comp a b) = evalHeap a + evalHeap b
evalHeap (ProdC a b) = evalHeap a + evalHeap b
evalHeap (SumC a b) = evalHeap a + evalHeap b
evalHeap _ = 0

evalHeap' :: UPia a b -> Int
evalHeap' (FromUPi a) = evalHeap a

evalHeap'' :: UPichia a b -> Int
evalHeap'' (FromUPia a) = evalHeap' a


-- readable UPichia format
-- parse UPi
toUPia :: UPi (Plus a h) b -> UPi a b
toUPia (Comp (Comp g (SumC f Id)) AssocPI) = Comp (toUPia g) (toUPia f)
toUPia (Comp (Comp (Comp (ProdC f g) (Comp SwapT (Comp DistribI (SumC SwapT SwapT)))) (SumC DistribI DistribI)) AssocPI) = ProdC (toUPia f) (toUPia g)
toUPia (Comp (SumC f g) (Comp (Comp (Comp (Comp AssocPI (SumC Id AssocP)) (SumC Id (SumC SwapP Id))) (SumC Id AssocPI)) AssocP)) = SumC (toUPia f) (toUPia g)
toUPia (Comp f UnitP) = f
toUPia UnitP = id
toUPia Id = UPiBase.Inl
toUPia SwapP = UPiBase.Inr

-- parse UPia
toUPichia :: UPi a (Times b g) -> UPi a b
toUPichia (Comp (Comp AssocT (ProdC g Id)) f) = Comp (toUPichia g) (toUPichia f)
toUPichia (Comp (Comp (Comp (Comp (Comp AssocTI (ProdC Id AssocT)) (ProdC Id (ProdC SwapT Id))) (ProdC Id AssocTI)) AssocT) (ProdC f g)) = ProdC (toUPichia f) (toUPichia g)
toUPichia (Comp UnitTI f) = f
toUPichia (Comp SwapT UnitTI) = UPiBase.Discard

-- helper function to handle multiplication of clone
-- 这个函数的type每次都报错 如果从clone的化简不知道该在哪一层进行 clone自己的乘法法则不知道该怎么化简
-- multiClone :: UPi a (Times a b) -> UPi a b
-- multiClone (Comp (Comp DistribI (SumC (ProdC Inl Id) (ProdC Inr Id))) (SumC UnitTI UnitTI)) = UPiBase.CloneQubit
-- multiClone (Comp (Comp (Comp (Comp (Comp AssocTI (ProdC Id AssocT)) (ProdC Id (ProdC SwapT Id))) (ProdC Id AssocTI)) AssocT) (ProdC f g)) = ProdC (multiClone f) (multiClone g)

-- a few constand for parsing phase in circuit format
minusI = -i
minus_1 = -1
pi_4 = exp(i*(pi/4))

-- parse to circuit format and measure
toCircuit :: UPi a b -> UPi a b
-- toCircuit (Comp (Comp DistribI (SumC (ProdC Inl Id) (ProdC Inr Id))) (SumC UnitTI UnitTI)) = UPiBase.CloneQubit
-- toCircuit (Comp (Comp (Comp (Comp (Comp AssocTI (ProdC Id AssocT)) (ProdC Id (ProdC SwapT Id))) (ProdC Id AssocTI)) AssocT) (ProdC f g)) = multiClone (Comp (Comp (Comp (Comp (Comp AssocTI (ProdC Id AssocT)) (ProdC Id (ProdC SwapT Id))) (ProdC Id AssocTI)) AssocT) (ProdC f g))
toCircuit SwapP = UPiBase.Px
toCircuit (Comp (SumC (Phase minusI) (Phase i)) SwapP) = UPiBase.Py
toCircuit (SumC Id (Phase minus_1)) = UPiBase.Pz
toCircuit (SumC Id (Phase i)) = UPiBase.PhaseS
toCircuit (SumC Id (Phase pi_4)) = UPiBase.PhaseT
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

-- eval the number of Qubits
evalNum :: UPi a b -> Int
evalNum (ProdC a b) = evalNum a + evalNum b
evalNum UPiBase.Id = 1
evalNum UPiBase.Hadamard = 1
evalNum UPiBase.Px = 1
evalNum UPiBase.Py = 1
evalNum UPiBase.Pz = 1
evalNum UPiBase.PhaseS = 1
evalNum UPiBase.PhaseT = 1
evalNum UPiBase.Cnot = 2
evalNum UPiBase.Toffoli = 3

-- total Qubit
totalQubit :: UPi a b -> Int
totalQubit (Comp a b) = evalNum b

-- eval the number of consecutive measurement (based on 3 assumptions)
-- 1. there is only one measurement in one layer (can rewrite by changing the order of qubits)
-- 2. for every Id operator there is only one corresponding Qubit
-- 3. the input is in the defined universal set of gates
-- 4. the first layer should not contain measure operator
evalGarbage :: Int -> UPi a b -> Int
evalGarbage x (Comp (ProdC (ProdC f Measure) g) (ProdC (ProdC p Measure) q)) = 
    let f' = evalNum f; g' = x - evalNum g; p' = evalNum p; q' = x - evalNum q
    in if and[f' < p', g' >= p', g' < q'] then g' - p' 
        else if and[f' < p', g' >= q'] then q' - p' 
            else if and[f' >= p', f' < q', g' > q'] then q' - f' 
                else if and[f' >= p', g' <= q'] then g' - f' 
                    else 0
evalGarbage x (Comp (ProdC Measure g) (ProdC (ProdC p Measure) q)) = 
    let f' = 0; g' = x - evalNum g; p' = evalNum p; q' = x - evalNum q
    in if and[f' < p', g' >= p', g' < q'] then g' - p' 
        else if and[f' < p', g' >= q'] then q' - p' 
            else if and[f' >= p', f' < q', g' > q'] then q' - f' 
                else if and[f' >= p', g' <= q'] then g' - f' 
                    else 0
evalGarbage x (Comp (ProdC f Measure) (ProdC (ProdC p Measure) q)) =
    let f' = evalNum f; g' = x; p' = evalNum p; q' = x - evalNum q
    in if and[f' < p', g' >= p', g' < q'] then g' - p' 
        else if and[f' < p', g' >= q'] then q' - p' 
            else if and[f' >= p', f' < q', g' > q'] then q' - f' 
                else if and[f' >= p', g' <= q'] then g' - f' 
                    else 0
evalGarbage x (Comp Measure (ProdC (ProdC p Measure) q)) = x - evalNum q - evalNum p

evalGarbage x (Comp (ProdC (ProdC f Measure) g) (ProdC Measure q)) = 
    let f' = evalNum f; g' = x - evalNum g; p' = 0; q' = x - evalNum q
    in if and[f' < p', g' >= p', g' < q'] then g' - p' 
        else if and[f' < p', g' >= q'] then q' - p' 
            else if and[f' >= p', f' < q', g' > q'] then q' - f' 
                else if and[f' >= p', g' <= q'] then g' - f' 
                    else 0
evalGarbage x (Comp (ProdC Measure g) (ProdC Measure q)) = 
    let g' = x - evalNum g; q' = x - evalNum q
    in if q' <= g' then q' else g'
evalGarbage x (Comp (ProdC f Measure) (ProdC Measure q)) = 
    let f' = evalNum f; q' = x - evalNum q
    in if f' <= q' then q' - f' else 0
evalGarbage x (Comp Measure (ProdC Measure q)) = x - evalNum q

evalGarbage x (Comp (ProdC (ProdC f Measure) g) (ProdC p Measure)) = 
    let f' = evalNum f; g' = x - evalNum g; p' = evalNum p; q' = x
    in if g' >= p' then g' - p' 
        else if and[f' < p', g' >= q'] then q' - p' 
            else if and[f' >= p', f' < q', g' > q'] then q' - f' 
                else if and[f' >= p', g' <= q'] then g' - f' 
                    else 0
evalGarbage x (Comp (ProdC Measure g) (ProdC p Measure)) = 
    let g' = x - evalNum g; p' = evalNum p
    in if g' >= p' then g' - p' else 0
evalGarbage x (Comp (ProdC f Measure) (ProdC p Measure)) =
    let f' = evalNum f; p' = evalNum p
    in if f' >= p' then x - f' else x - p'
evalGarbage x (Comp Measure (ProdC p Measure)) = x - evalNum p

evalGarbage x (Comp (ProdC (ProdC f Measure) g) Measure) = x - evalNum g - evalNum f
evalGarbage x (Comp (ProdC Measure g) Measure) = x - evalNum g
evalGarbage x (Comp (ProdC f Measure) Measure) = x - evalNum f
evalGarbage x (Comp Measure Measure) = x

evalGarbage x (Comp (Comp a b) c) = evalGarbage x (Comp a b) + evalGarbage x (Comp b c)
evalGarbage x _ = 0

-- eval the number of consecutive measurement
evalConsecutive :: UPichia a b -> Int
evalConsecutive x = evalGarbage (totalQubit (toReadable' x)) (toReadable' x)

-- eval the number of ancilla
evalAncilla :: UPichia a b -> Int
evalAncilla x = evalHeap'' x - evalConsecutive x

-- A slightly optimzed version of UPia.arr' . UPi.arr'.
gate :: UPi a b -> UPichia a b
gate f = FromUPia (FromUPi (UPi.unitp >>> f >>> UPi.unitti))

-- test examples
-- test passed
ex2 = gate UPi.h >>> measure
ex3 :: UPichia ('Times Qbit Qbit) ('Times Qbit Qbit)
ex3 = gate UPi.h UPichia.**** gate UPi.h >>> measure
ex4 = gate UPi.h UPichia.**** gate UPi.h >>> id UPichia.**** measure >>> measure
ex5 = gate UPi.h UPichia.**** gate UPi.h >>> measure >>> measure
ex6 = gate UPi.h UPichia.**** gate UPi.h >>> id UPichia.**** measure >>> id UPichia.**** measure >>> id UPichia.**** measure
ex7 = gate UPi.h UPichia.**** gate UPi.h UPichia.**** gate UPi.h >>> measure UPichia.**** id >>> id UPichia.**** measure
ex8 = gate UPi.h UPichia.**** gate UPi.h UPichia.**** gate UPi.h >>> id UPichia.**** id UPichia.**** measure >>> gate UPi.h UPichia.**** measure UPichia.**** gate UPi.h >>> measure
ex9 = gate UPi.h UPichia.**** gate UPi.h >>> measure >>> measure >>> measure
ex10 = gate UPi.h UPichia.**** gate UPi.cnot UPichia.**** gate UPi.h >>> gate UPi.h UPichia.**** measure UPichia.**** gate UPi.h >>> measure UPichia.**** gate UPi.h
ex11 = gate UPi.h UPichia.**** gate UPi.cnot UPichia.**** gate UPi.h >>> id UPichia.**** (id UPichia.**** id) UPichia.**** measure >>> gate UPi.h UPichia.**** measure UPichia.**** gate UPi.h
-- test failed
ex12 = gate UPi.h UPichia.**** gate UPi.h UPichia.**** gate UPi.h >>> id UPichia.**** id UPichia.**** measure >>> id UPichia.**** measure UPichia.**** id >>> measure
-- can only solve measurement on consecutive layer. not for further layer
ex13 = gate UPi.h UPichia.**** (gate UPi.h UPichia.**** gate UPi.h) UPichia.**** gate UPi.h >>> gate UPi.h UPichia.**** measure UPichia.**** gate UPi.h
ex14 = gate UPi.h UPichia.**** gate UPi.h **** gate UPi.h **** (gate UPi.h **** gate UPi.h) >>> gate UPi.h **** measure **** gate UPi.h **** measure
