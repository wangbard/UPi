module Example where

import Control.Category
import Prelude hiding (id,(.))

import UPiBase
import UPiaBase
import UPichiaBase
import UPi hiding ((****),(++++))
import UPia hiding ((****),(++++))
import UPichia

-- A slightly optimzed version of UPia.arr' . UPi.arr'.
gate :: UPi a b -> UPichia a b
gate f = FromUPia (FromUPi (UPi.unitp >>> f >>> UPi.unitti))

-- An example circuit with measurement (corresponding to examples/ex1.pdf).
ex1 = (id **** gate UPi.h **** id) >>> 
      (gate UPi.cnot **** id) >>> 
      (id **** gate UPi.h **** id) >>>
      UPichia.assoct >>>
      (id **** gate UPi.cnot) >>> 
      UPichia.assocti >>>
      (id **** id **** gate UPi.pz) >>> 
      (id **** measure **** measure)


-- a hand written parse tree for the following simple UPichia program
ex3 = gate UPi.h >>> measure 

{-    (Hadamard >>> UnitTI) >>> (A >>> AssocT)

AssocPI
(UnitP >>> Hadamard >>> UnitTI) ++++ Id
AssocPI
A ++++ Id
--A   B **** Id
      AssocPI
      DistribI ++++ DistribI
      SwapT ++++ SwapT
      DistribI
      SwapT
      B **** UnitP
      --B   C >>> (J >>> AssocT)
            AssocPI
            C ++++ Id
            --C   D >>> UnitTI
                  AssocPI
                  D ++++ Id
                  --D   E >>> (F >>> DistribI)
                        AssocPI
                        E ++++ Id
                        --E   clone1 ++++ clone1
                              AssocP
                              Id ++++ AssocPI
                              Id ++++ (SwapP ++++ Id)
                              Id ++++ AssocP
                              AssocPI
                              (UnitP >>> UnitTI)  ++++  (UnitP >>> UnitTI)
                        --E
                        AssocPI
                        F ++++ Id
                        --F   (inl **** Id) ++++ (inr **** Id)
                              AssocP
                              Id ++++ AssocPI
                              Id ++++ (SwapP ++++ Id)
                              Id ++++ AssocP
                              AssocPI
                              G ++++ H
                              --G   inl **** Id
                                    AssocPI
                                    DistribI ++++ DistribI
                                    SwapT ++++ SwapT
                                    DistribI
                                    SwapT
                                    Id **** UnitP
                              --G
                              --H   inr **** Id
                                    AssocPI
                                    DistribI ++++ DistribI
                                    SwapT ++++ SwapT
                                    DistribI
                                    SwapT
                                    SwapP **** UnitP
                              --H
                        --F
                        UnitP >>> DistribI
                  --D
                  UnitP >>> UnitTI
            --C
            AssocPI
            J ++++ Id
            --J   K **** Id
                  AssocPI
                  DistribI ++++ DistribI
                  SwapT ++++ SwapT
                  DistribI
                  SwapT
                  K **** UnitP
                  --K   L >>> (Q >>> AssocT)
                        AssocPI
                        L ++++ Id
                        --L   M >>> lift(X)
                              AssocPI
                              M ++++ Id
                              --M   (UnitTI >>> SwapT) **** (Id >>> UnitTI)
                                    AssocPI
                                    DistribI ++++ DistribI
                                    SwapT ++++ SwapT
                                    DistribI
                                    SwapT
                                    N **** P
                                    --N   
                                          UnitP >>> UnitTI >>> SwapT
                                    --N
                                    --P   Id >>> UnitTI
                                          AssocPI
                                          UnitP ++++ Id
                                          UnitP >>> UnitTI
                                    --P
                              --M
                              X
                              --X
                                    UnitP
                                    AssocT
                                    Id **** AssocTI
                                    Id **** (SwapT **** Id)
                                    Id **** AssocT
                                    AssocTI
                              --X
                        --L
                        AssocPI
                        Q ++++ Id
                        --Q   (UnitT >>> UnitTI) **** Id
                              AssocPI
                              DistribI ++++ DistribI
                              SwapT ++++ SwapT
                              DistribI
                              SwapT
                              R **** UnitP
                              --R   UnitT >>> UnitTI
                                    AssocPI
                                    (UnitP >>> UnitT)  ++++  Id
                                    UnitP >>> UnitTI
                              --R
                        --Q
                        UnitP >>> AssocT
                  --K
            --J
            UnitP >>> AssocT
      --B
--A
UnitP >>> AssocT
-}

{-    
      Hadamard >>> 
      clone(qbit) >>>
      fst

(Hadamard >>> UnitTI) >>> (A >>> AssocT)
--A   
      B **** Id
      --B   C >>> K
            C >>> (J >>> AssocT)
            --C   clone(qbit)
                  D >>> UnitTI
                  --D   clone(qbit) (not lifted)
                        E >>> (F >>> DistribI)
                        --E   
                              clone1 ++++ clone1
                        --E
                        
                        --F   
                              (inl **** Id) ++++ (inr **** Id)
                        --F   
                  --D     
            --C
            --J   
                  K **** Id
                  --K   fst
                        L >>> (Q >>> AssocT)
                        --L   Id **** discard
                              M
                              --M   
                                    (Id >>> UnitTI) **** (UnitTI >>> SwapT)
                              --M
                              AssocT
                              Id **** AssocTI
                              Id **** (SwapT **** Id)
                              Id **** AssocT
                              AssocTI
                        --L
                        --Q   
                              (UnitT >>> UnitTI) **** Id
                        --Q
                  --K
            --J
      --B
--A
-}