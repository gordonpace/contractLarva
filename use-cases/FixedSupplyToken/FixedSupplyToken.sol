//FROM: https://theethereum.wiki/w/index.php/ERC20_Token_Standard
//EDITED: Shaun Azzopardi, July 2018

pragma solidity ^0.4.24;

// ----------------------------------------------------------------------------
// 'FIXED' 'Example Fixed Supply Token' token contract
//
// Symbol      : FIXED
// Name        : Example Fixed Supply Token
// Total supply: 1,000,000.000000000000000000
// Decimals    : 18
//
// Enjoy.
//
// (c) BokkyPooBah / Bok Consulting Pty Ltd 2018. The MIT Licence.
// ----------------------------------------------------------------------------


// ----------------------------------------------------------------------------
// Safe maths
// ----------------------------------------------------------------------------
// library SafeMath {
//     function add(uint a, uint b) internal pure returns (uint c) {
//         c = a + b;
//         require(c >= a);
//     }
//     function sub(uint a, uint b) internal pure returns (uint c) {
//         require(b <= a);
//         c = a - b;
//     }
//     function mul(uint a, uint b) internal pure returns (uint c) {
//         c = a * b;
//         require(a == 0 || c / a == b);
//     }
//     function div(uint a, uint b) internal pure returns (uint c) {
//         require(b > 0);
//         c = a / b;
//     }
// }


// ----------------------------------------------------------------------------
// ERC Token Standard #20 Interface
// https://github.com/ethereum/EIPs/blob/master/EIPS/eip-20.md
// ----------------------------------------------------------------------------
interface ERC20TokenImplementation {
  function totalSupply () public constant returns (uint);
  function balanceOf (address tokenOwner) public constant returns (uint balance);
  function allowance (address tokenOwner, address spender) public constant returns (uint remaining);
  function transfer (address caller, address to, uint tokens) public returns (bool success);
  function approve (address caller, address spender, uint tokens) public returns (bool success);
  function transferFrom (address caller, address from, address to, uint tokens) public returns (bool success);
  event Transfer (address indexed from, address indexed to, uint tokens);
  event Approval (address indexed tokenOwner, address indexed spender, uint tokens);

}



// ----------------------------------------------------------------------------
// Contract function to receive approval and execute function in one call
//
// Borrowed from MiniMeToken
// ----------------------------------------------------------------------------
contract ApproveAndCallFallBack {
    function receiveApproval(address from, uint256 tokens, address token, bytes data) public;
}


// ----------------------------------------------------------------------------
// Owned contract
// ----------------------------------------------------------------------------
contract Owned {
    address public owner;
    address public newOwner;

    event OwnershipTransferred(address indexed _from, address indexed _to);

    constructor() public {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }

    function transferOwnership(address _newOwner) public onlyOwner {
        owner = _newOwner;
    }
}


// ----------------------------------------------------------------------------
// ERC20 Token, with the addition of symbol, name and decimals and a
// fixed supply
// ----------------------------------------------------------------------------
contract FixedSupplyToken is ERC20TokenImplementation, Owned {

    string public symbol;
    string public  name;
    uint8 public decimals;
    uint _totalSupply;

    mapping(address => uint) balances;
    mapping(address => mapping(address => uint)) allowed;


    // ------------------------------------------------------------------------
    // Constructor
    // ------------------------------------------------------------------------
    constructor() public {
        symbol = "FIXED";
        name = "Example Fixed Supply Token";
        decimals = 18;
        _totalSupply = 1000000 * 10**uint(decimals);
        balances[owner] = _totalSupply;
        emit Transfer(address(0), owner, _totalSupply);
    }


    // ------------------------------------------------------------------------
    // Total supply
    // ------------------------------------------------------------------------
    function totalSupply() public view returns (uint) {
        return sub(_totalSupply,balances[address(0)]);
    }


    // ------------------------------------------------------------------------
    // Get the token balance for account `tokenOwner`
    // ------------------------------------------------------------------------
    function balanceOf(address tokenOwner) public view returns (uint balance) {
        return balances[tokenOwner];
    }


    // ------------------------------------------------------------------------
    // Transfer the balance from token owner's account to `to` account
    // - Owner's account must have sufficient balance to transfer
    // - 0 value transfers are allowed
    // ------------------------------------------------------------------------
    function transfer(address caller, address to, uint tokens) onlyOwner public returns (bool success) {
        balances[caller] = sub(balances[caller],tokens);
        balances[to] = add(balances[to],tokens);
        emit Transfer(caller, to, tokens);
        return true;
    }


    // ------------------------------------------------------------------------
    // Token owner can approve for `spender` to transferFrom(...) `tokens`
    // from the token owner's account
    //
    // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-20-token-standard.md
    // recommends that there are no checks for the approval double-spend attack
    // as this should be implemented in user interfaces 
    // ------------------------------------------------------------------------
    function approve(address caller, address spender, uint tokens) onlyOwner public returns (bool success) {
        allowed[caller][spender] = tokens;
        emit Approval(caller, spender, tokens);
        return true;
    }


    // ------------------------------------------------------------------------
    // Transfer `tokens` from the `from` account to the `to` account
    // 
    // The calling account must already have sufficient tokens approve(...)-d
    // for spending from the `from` account and
    // - From account must have sufficient balance to transfer
    // - Spender must have sufficient allowance to transfer
    // - 0 value transfers are allowed
    // ------------------------------------------------------------------------
    function transferFrom(address caller, address from, address to, uint tokens) onlyOwner public returns (bool success) {
        balances[from] = sub(balances[from], tokens);
        allowed[from][caller] = sub(allowed[from][caller], tokens);
        balances[to] = add(balances[to], tokens);
        emit Transfer(from, to, tokens);
        return true;
    }


    // ------------------------------------------------------------------------
    // Returns the amount of tokens approved by the owner that can be
    // transferred to the spender's account
    // ------------------------------------------------------------------------
    function allowance(address tokenOwner, address spender) public view returns (uint remaining) {
        return allowed[tokenOwner][spender];
    }


    // ------------------------------------------------------------------------
    // Token owner can approve for `spender` to transferFrom(...) `tokens`
    // from the token owner's account. The `spender` contract function
    // `receiveApproval(...)` is then executed
    // ------------------------------------------------------------------------
    function approveAndCall(address caller, address spender, uint tokens, bytes data) onlyOwner public returns (bool success) {
        allowed[caller][spender] = tokens;
        emit Approval(caller, spender, tokens);
        ApproveAndCallFallBack(spender).receiveApproval(caller, tokens, this, data);
        return true;
    }


    // ------------------------------------------------------------------------
    // Don't accept ETH
    // ------------------------------------------------------------------------
    function () public payable {
        revert();
    }


    // ------------------------------------------------------------------------
    // Owner can transfer out any accidentally sent ERC20 tokens
    // ------------------------------------------------------------------------
    function transferAnyERC20Token(address tokenAddress, uint tokens) public onlyOwner returns (bool success) {
        return ERC20TokenImplementation(tokenAddress).transfer(msg.sender, owner, tokens);
    }

    function add(uint a, uint b) internal view returns (uint c) {
        c = a + b;
        require(c >= a);
    }
    function sub(uint a, uint b) internal view returns (uint c) {
        require(b <= a);
        c = a - b;
    }
    function mul(uint a, uint b) internal view returns (uint c) {
        c = a * b;
        require(a == 0 || c / a == b);
    }
    function div(uint a, uint b) internal view returns (uint c) {
        require(b > 0);
        c = a / b;
    }
}
