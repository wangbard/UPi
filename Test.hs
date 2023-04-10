module Test where

import Control.Category
import Prelude hiding (id,(.))

import UPiBase
import UPiaBase
import UPichiaBase
import UPi hiding ((****),(++++))
import UPia hiding ((****),(++++))
import UPichia
import Ancilla (evalAncilla, noConsecutiveMProgram, singleQubitMProgram)


-- this module contains the test set for ancilla.hs

-- ancilla.hs evaluates the number of ancilla used in a UPichia program based on 3 assumptions on the input
-- 1. for every Id operator there is only one corresponding Qubit
-- 2. the input is in quantum circuit format, i.e. in a defined universal set of gates
-- 3. the first layer should not contain measure operator
-- 4. the layer that is dedicated to the change of parens (namely layer that contains UPichia.assoct or UPichia.assocti) should not contain any gates. 
--    This is to say that this layer should only contain a product of {UPichia.assoct, UPichia.assocti, Id}.

-- the module is able to output:
-- 1. evalAncilla : return the number of ancilla
-- 2. noConsecutiveMProgram : return a readable UPichia program that contains no consecutive measurement
-- 3. singleQubitMProgram : return a reabable UPichia program that expand a compound measurement (measurement on multiple Qubits) into measurements on single Qubit


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
ex21 = gate UPi.px **** gate UPi.py **** gate UPi.pz **** gate UPi.s **** gate UPi.t >>>
       measure **** id >>>
       measure **** id **** id **** id **** id
ex22 = (gate UPi.cnot **** gate UPi.s) **** (gate UPi.h **** gate UPi.h) **** gate UPi.h >>>
       measure **** (id **** id) **** id >>>
       UPichia.assoct >>>
       ((id **** id) **** id) **** measure >>>
       id **** UPichia.assoct >>>
       measure **** (id **** measure)
ex23 = (gate UPi.cnot **** gate UPi.s) **** (gate UPi.t **** gate UPi.px) **** gate UPi.h >>>
       measure **** (id **** id) **** id >>>
       UPichia.assoct >>>
       ((id **** id) **** id) **** (gate UPi.cnot **** id) >>>
       id **** UPichia.assoct >>>
       measure **** (id **** measure)
ex24 = (gate UPi.toffoli **** gate UPi.py) **** gate UPi.pz >>>
       measure **** id >>>
       UPichia.assoct >>>
       measure **** gate UPi.cnot >>>
       measure
ex25 = (gate UPi.px **** gate UPi.s **** gate UPi.cnot) **** gate UPi.t >>>
       measure **** id >>>
       UPichia.assoct **** id >>>
       (id **** gate UPi.toffoli) **** measure >>>
       measure