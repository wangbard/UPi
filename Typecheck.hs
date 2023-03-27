{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE TypeFamilies #-}

module Typecheck where

import UPi
import UPiBase

-- The empty type
data Void

type family Interpret a where
    Interpret O = Void
    Interpret I = ()
    Interpret (Times a b) = (Interpret a, Interpret b)
    Interpret (Plus a b) = Either (Interpret a) (Interpret b)

interpret :: UPi a b -> Interpret a -> Interpret b
interpret Id x = x
interpret SwapP (Left x) = Right x
interpret SwapP (Right y) = Left y
interpret AssocP (Left (Left x)) = Left x
interpret AssocP (Left (Right y)) = Right (Left y)
interpret AssocP (Right z) = Right (Right z)
interpret AssocPI (Left x) = Left (Left x)
interpret AssocPI (Right (Left y)) = Left (Right y)
interpret AssocPI (Right (Right z)) = Right z
interpret UnitP (Left x) = x
interpret UnitPI x = Left x
interpret SwapT (x,y) = (y,x)
interpret AssocT ((x,y),z) = (x,(y,z))
interpret AssocTI (x,(y,z)) = ((x,y),z)
interpret UnitT (x,()) = x
interpret UnitTI x = (x,())
interpret Distrib (x, Left y) = Left (x,y)
interpret Distrib (x, Right z) = Right (x,z)
interpret DistribI (Left (x,y)) = (x, Left y)
interpret DistribI (Right (x,z)) = (x, Right z)
interpret Hadamard x = x
interpret (Phase _) x = x
interpret (Comp t1 t2) x = let g = interpret t1; f = interpret t2 in g (f x)
interpret (SumC t1 t2) (Left x) = Left (interpret t1 x)
interpret (SumC t1 t2) (Right y) = Right (interpret t2 y)
interpret (ProdC t1 t2) (x,y) = (interpret t1 x, interpret t2 y)