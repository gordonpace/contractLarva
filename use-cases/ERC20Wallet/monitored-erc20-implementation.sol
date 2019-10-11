pragma solidity ^0.4.24;
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
contract ApproveAndCallFallBack {
  function receiveApproval (address from, uint256 tokens, address token, bytes data) public;

}
contract Owned {
  address public owner;
  address public newOwner;
  event OwnershipTransferred (address indexed _from, address indexed _to);
  constructor () public {
    owner = msg.sender;
  }
  modifier onlyOwner {
    require(msg.sender == owner);
    _;
  }
  function transferOwnership (address _newOwner) public onlyOwner {
    owner = _newOwner;
  }

}
contract LARVA_FixedSupplyToken is ERC20TokenImplementation, Owned {
  modifier LARVA_DEA_1_handle_after_transfer__parameters_caller_to_tokens (address caller, address to, uint tokens) {
    if ((LARVA_STATE_1 == 4)) {
      LARVA_STATE_1 = 0;
      currentTokens = 0;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_sub__parameters_a_tokens (uint a, uint tokens) {
    if ((LARVA_STATE_1 == 1) && (currentTokens == tokens)) {
      LARVA_STATE_1 = 3;
    } else {
      if ((LARVA_STATE_1 == 1) && (currentTokens != tokens)) {
        LARVA_STATE_1 = 5;
        LARVA_reparation();
      } else {
        if ((LARVA_STATE_1 == 2) && (currentTokens == tokens)) {
          LARVA_STATE_1 = 4;
        } else {
          if ((LARVA_STATE_1 == 2) && (currentTokens != tokens)) {
            LARVA_STATE_1 = 5;
            LARVA_reparation();
          } else {
            if ((LARVA_STATE_1 == 2)) {
              LARVA_STATE_1 = 5;
              LARVA_reparation();
            }
          }
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_add__parameters_a_tokens (uint a, uint tokens) {
    if ((LARVA_STATE_1 == 1) && (currentTokens == tokens)) {
      LARVA_STATE_1 = 2;
    } else {
      if ((LARVA_STATE_1 == 1) && (currentTokens != tokens)) {
        LARVA_STATE_1 = 5;
        LARVA_reparation();
      } else {
        if ((LARVA_STATE_1 == 2)) {
          LARVA_STATE_1 = 5;
          LARVA_reparation();
        } else {
          if ((LARVA_STATE_1 == 2) && (currentTokens == tokens)) {
            LARVA_STATE_1 = 4;
          } else {
            if ((LARVA_STATE_1 == 2) && (currentTokens != tokens)) {
              LARVA_STATE_1 = 5;
              LARVA_reparation();
            }
          }
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_1_handle_before_transfer__parameters_caller_to_tokens (address caller, address to, uint tokens) {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 1;
      currentTokens = tokens;
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  function LARVA_reparation () private {
    revert();
  }
  uint currentTokens;
  modifier LARVA_Constructor {
    {
    }
    LARVA_Status = LARVA_STATUS.RUNNING;
    _;
  }
  enum LARVA_STATUS {NOT_STARTED, READY, RUNNING, STOPPED}
  function LARVA_EnableContract () LARVA_ContractIsEnabled private {
    LARVA_Status = (LARVA_Status == LARVA_STATUS.NOT_STARTED)?LARVA_STATUS.READY:LARVA_STATUS.RUNNING;
  }
  function LARVA_DisableContract () LARVA_ContractIsEnabled private {
    LARVA_Status = (LARVA_Status == LARVA_STATUS.READY)?LARVA_STATUS.NOT_STARTED:LARVA_STATUS.STOPPED;
  }
  LARVA_STATUS private LARVA_Status = LARVA_STATUS.NOT_STARTED;
  modifier LARVA_ContractIsEnabled {
    require(LARVA_Status == LARVA_STATUS.RUNNING);
    _;
  }
  string public symbol;
  string public name;
  uint8 public decimals;
  uint _totalSupply;
  mapping (address => uint) balances;
  mapping (address => mapping (address => uint)) allowed;
  constructor () LARVA_Constructor public {
    symbol = "FIXED";
    name = "Example Fixed Supply Token";
    decimals = 18;
    _totalSupply = 1000000 * 10 ** uint(decimals);
    balances[owner] = _totalSupply;
    emit Transfer(address(0), owner, _totalSupply);
  }
  function totalSupply () LARVA_ContractIsEnabled public view returns (uint) {
    return sub(_totalSupply, balances[address(0)]);
  }
  function balanceOf (address tokenOwner) LARVA_ContractIsEnabled public view returns (uint balance) {
    return balances[tokenOwner];
  }
  function transfer (address caller, address to, uint tokens) LARVA_DEA_1_handle_after_transfer__parameters_caller_to_tokens(caller, to, tokens) LARVA_DEA_1_handle_before_transfer__parameters_caller_to_tokens(caller, to, tokens) LARVA_ContractIsEnabled onlyOwner public returns (bool success) {
    balances[caller] = sub(balances[caller], tokens);
    balances[to] = add(balances[to], tokens);
    emit Transfer(caller, to, tokens);
    return true;
  }
  function approve (address caller, address spender, uint tokens) LARVA_ContractIsEnabled onlyOwner public returns (bool success) {
    allowed[caller][spender] = tokens;
    emit Approval(caller, spender, tokens);
    return true;
  }
  function transferFrom (address caller, address from, address to, uint tokens) LARVA_ContractIsEnabled onlyOwner public returns (bool success) {
    balances[from] = sub(balances[from], tokens);
    allowed[from][caller] = sub(allowed[from][caller], tokens);
    balances[to] = add(balances[to], tokens);
    emit Transfer(from, to, tokens);
    return true;
  }
  function allowance (address tokenOwner, address spender) LARVA_ContractIsEnabled public view returns (uint remaining) {
    return allowed[tokenOwner][spender];
  }
  function approveAndCall (address caller, address spender, uint tokens, bytes data) LARVA_ContractIsEnabled onlyOwner public returns (bool success) {
    allowed[caller][spender] = tokens;
    emit Approval(caller, spender, tokens);
    ApproveAndCallFallBack(spender).receiveApproval(caller, tokens, this, data);
    return true;
  }
  function () public payable {
    revert();
  }
  function transferAnyERC20Token (address tokenAddress, uint tokens) LARVA_ContractIsEnabled public onlyOwner returns (bool success) {
    return ERC20TokenImplementation(tokenAddress).transfer(msg.sender, owner, tokens);
  }
  function add (uint a, uint b) LARVA_DEA_1_handle_after_add__parameters_a_tokens(a, b) LARVA_ContractIsEnabled internal view returns (uint c) {
    c = a + b;
    require(c >= a);
  }
  function sub (uint a, uint b) LARVA_DEA_1_handle_after_sub__parameters_a_tokens(a, b) LARVA_ContractIsEnabled internal view returns (uint c) {
    require(b <= a);
    c = a - b;
  }
  function mul (uint a, uint b) LARVA_ContractIsEnabled internal view returns (uint c) {
    c = a * b;
    require(a == 0 || c / a == b);
  }
  function div (uint a, uint b) LARVA_ContractIsEnabled internal view returns (uint c) {
    require(b > 0);
    c = a / b;
  }

}
