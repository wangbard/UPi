$Ancilla.hs$ contains functions used to calculate ancilla qubits and a number of test (passed/failed). You can call the test examples by running *evalAncilla ex2* (ex2-ex12 are tests for ancilla). You can also new some tests by yourself to test potential drawback. However, there are a few assumption to make about the input:
1. there is only one measurement in one layer (can rewrite by changing the order of qubits)
2. for every Id operator there is only one corresponding Qubit
3. the input is in the defined universal set of gates (Hadamard, Pauli x, Pauli y, Pauli z, S, T, Cnot, Toffoli) (This is a combination of many universal set of gates)
4. the first layer should not contain measure operator (In practice, measurement in the first layer is also meanningless as we normally do know the initial state of the qubit).

Layer, in the above description, means a vertical layer in quantum circuit where there is some operator for every qubits, or a horizontal layer for UPichia, which is a product of some operators.
