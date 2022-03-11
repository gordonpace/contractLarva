contract WalletWithDeposit{
    
    uint deposit;
    address payable owner;
    struct Provider {
        address payable wallet;
        string name;
    }
    
    Provider depositProvider;
    mapping(address => string) history;
    
    modifier iss(address _owner) {
        require(msg.sender == _owner);
        _;
    }
    
    constructor(string memory name, address payable _depositProvider) payable {
        require(msg.value > 0, "Must be started with a deposit.");
        (owner, deposit) = (payable(msg.sender), msg.value);
        depositProvider = Provider(_depositProvider, name);
        history[_depositProvider] = name;
    }
    
    function transfer(address payable to, uint tokens) iss(owner) external returns (bool success){
        to.transfer(tokens);
    }
    
    function changeOwnerAndDepositProvider (address payable _owner, string memory name, address payable _depositProvider) external iss(_depositProvider) returns (bool success){
        (owner, depositProvider) = (_owner, Provider(_depositProvider, name));
        history[_depositProvider] = name;
    }
    
    function end () external iss(owner) returns (bool success){
        depositProvider.wallet.transfer(deposit);
        selfdestruct(owner);
    }
}