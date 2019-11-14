This folder contains several Solidity smart contracts and specifications for them, intended both as use cases for users of contractLarva and to serve as a test suite for contributors to contractLarva.

Each folder __\Name__ is of the following form:
    -__contractLarva\use-cases\Name\Name.sol__ : The original smart contract.
    -__contractLarva\use-cases\Name\NameSpec.dea__: The contractLarva specification for the smart contract.
    -__contractLarva\use-cases\Name\MonitoredName.sol__: The smart contract monitored with the specification.

A note is that some of the examples may be verifiable by hand or by some light static analysis, while others actually functionality to the smart contract. In contractLarva these are all monitored at runtime, making it less than ideal for statically verifiable use cases. We suggest performing static analysis through other means to reduce runtime overheads.

Below is a short description of each of these use cases:

1. **Casino**: An implementation of a guessing game in Solidity, were the owner is trusted. When __openTable__ is called the users can start betting with __placeBet__, with the amount they bet placed in a __pot__. The owner can stop a game with __resolveBet__, where the winners divide the __pot__ amongst them. Or the owner can __timeoutBet__, where the users are refunded their bets. The specification for the casino specifies two behaviours: (i) While the table is open the pot cannot reduce in value; and (ii) the table remains open until resolution or timeout.

2. **CourierService**: An implementation of a courier service contract, where an __order__ and its __delivery__ can be recorded on the blockchain, where if the __order__ is not received on time the client can cancel the order and __requestRefund__ automatically. The specification specifies the correct functional behaviour of this implementation, e.g. ensuring the correct customer is recorded for an __order__, and detecting violation when there is a __delivery__ or __refund__ before there has been an __order__, or when another __order__ is attempted (the smart contract is intended to be used for one order).

3. **ERC20Interface**: An implementation of the ERC20 token interface, allowing __transfer__ of tokens. This interface allows for the underlying implementation to be changed at runtime since it performs its functionality by passing on calls to the recorded __impl__ address. The specification specifies the functional contracts required by the ERC20 standard and that must be respected by implementations at runtime.

4. **FixedSupplyToken**: An example implementation of an **ERC20Interface** which relies on the supply of tokens not changing. The specification ensures this by specifying that any addition (or subtraction) to a user's wallet must be followed by a substraction (or addition) of the same value to a user's wallet before the __transfer__ function has finished.

5. 