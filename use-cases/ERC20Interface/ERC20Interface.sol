pragma solidity ^0.4.26;

// ----------------------------------------------------------------------------
// ERC Token Standard #20 Interface
// https://github.com/ethereum/EIPs/blob/master/EIPS/eip-20-token-standard.md
// ----------------------------------------------------------------------------
interface ERC20TokenImplementation {
  function totalSupply () external constant returns (uint);
  function balanceOf (address tokenOwner) external constant returns (uint balance);
  function allowance (address tokenOwner, address spender) external constant returns (uint remaining);
  function transfer (address caller, address to, uint tokens) external returns (bool success);
  function approve (address caller, address spender, uint tokens) external returns (bool success);
  function transferFrom (address caller, address from, address to, uint tokens) external returns (bool success);
  event Transfer (address indexed from, address indexed to, uint tokens);
  event Approval (address indexed tokenOwner, address indexed spender, uint tokens);

}
contract ERC20Interface{
    
    ERC20TokenImplementation impl;
    address owner;

    constructor(ERC20TokenImplementation _impl, address _owner) public{
        impl = _impl;
        owner = _owner;
    }
    
    function updateImplementation(address newImpl) public{
        require(msg.sender == owner);
        impl = ERC20TokenImplementation(newImpl);
    }
    
    function totalSupply() public constant returns (uint){
        return impl.totalSupply();
    }
    
    function balanceOf(address tokenOwner) public constant returns (uint balance){
        return impl.balanceOf(tokenOwner);
    }
    
    function allowance(address tokenOwner, address spender) public constant returns (uint remaining){
        return impl.allowance(tokenOwner, spender);
    }
    
    function transfer(address to, uint tokens) public returns (bool success){
        return impl.transfer(msg.sender, to, tokens);    
    }
    
    function approve(address spender, uint tokens) public returns (bool success){
        return impl.approve(msg.sender, spender, tokens);    
    }
    
    function transferFrom(address from, address to, uint tokens) public returns (bool success){
        return impl.transferFrom(msg.sender, from, to, tokens);
    }
}