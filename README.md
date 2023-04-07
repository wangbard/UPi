To see the result of the code, run **ghci Test.hs**

***Ancilla.hs*** contains functions used to calculate ancilla qubits. 
***Test.hs*** contains a number of tests.

You can call three functions in ***Test.hs***:

1. **evalAncilla** : return the number of ancilla

2. **noConsecutiveMProgram** : return a readable UPichia program that contains no consecutive measurement

3. **singleQubitMProgram** : return a reabable UPichia program that expand a compound measurement (measurement on multiple Qubits) into measurements on single Qubit

You can call the test examples by running **evalAncilla ex1** (ex1-ex21 are tests for ancilla). You can also new some tests by yourself to test potential drawback. Note that there are a few simple assumptions to make about the input:

1. for every Id operator there is only one corresponding Qubit.
2. the input is in the defined universal set of gates (Hadamard, Pauli X, Pauli Y, Pauli Z, S, T, Cnot, Toffoli) (This is a combination of some universal set of gates).
3. the first layer should not contain measure operator (In practice, measurement in the first layer is also meaningless as we normally do know the initial state of the qubits).
Note that in the layer that does not contains **UPichia.assoct** or **UPichia.assocti**, which is the layer that controls the change of parens, you don't need to follow rule 1. So the **Id** operator in this layer has type **UPichia a a** which can be mapped to arbitrary UPichia term.

Layer, in the above description, means a horizontal layer in the input UPichia program which is a just product of some operators without any composition.

To take a closer look at what UPichia program does and what quantum information effects mean, please refer to [rkaarsgaard/UPi](https://github.com/rkaarsgaard/upi).