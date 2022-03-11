contract LARVA_WalletWithDeposit {
  modifier LARVA_Constructor {
    _;
    {
      LARVA_EnableContract();
    }
  }
  modifier LARVA_DEA_1_handle_after_assignment_history {
    _;
    if ((LARVA_STATE_1 == 0) && (history[depositProvider.wallet] != depositProvider.name)) {
      LARVA_STATE_1 = 5;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_1_handle_after_assignment_depositProvider {
    _;
    if ((LARVA_STATE_1 == 0) && (owner == depositProvider.wallet)) {
      LARVA_STATE_1 = 4;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_1_handle_after_assignment_deposit {
    _;
    if ((LARVA_STATE_1 == 0) && (true)) {
      LARVA_STATE_1 = 3;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_1_handle_after_transfer {
    _;
    if ((LARVA_STATE_1 == 0) && (address(this).balance >= deposit)) {
      LARVA_STATE_1 = 2;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_1_handle_before_end__parameters_ () {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 1;
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  function LARVA_set_deposit_pre (uint _deposit) LARVA_DEA_1_handle_after_assignment_deposit internal returns (uint) {
    LARVA_previous_deposit = deposit;
    deposit = _deposit;
    return LARVA_previous_deposit;
  }
  function LARVA_set_deposit_post (uint _deposit) LARVA_DEA_1_handle_after_assignment_deposit internal returns (uint) {
    LARVA_previous_deposit = deposit;
    deposit = _deposit;
    return deposit;
  }
  uint private LARVA_previous_deposit;
  function LARVA_set_depositProvider_pre_wallet (address payable _value) internal returns (address payable) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider.wallet = _value;
    return LARVA_previous_depositProvider;
  }
  function LARVA_set_depositProvider_pre_name (string _value) internal returns (string) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider.name = _value;
    return LARVA_previous_depositProvider;
  }
  function LARVA_set_depositProvider_pre (Provider _depositProvider) LARVA_DEA_1_handle_after_assignment_depositProvider internal returns (Provider) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider = _depositProvider;
    return LARVA_previous_depositProvider;
  }
  function LARVA_set_depositProvider_pre_wallet (address payable _value) internal returns (address payable) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider.wallet = _value;
    return depositProvider;
  }
  function LARVA_set_depositProvider_pre_name (string _value) internal returns (string) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider.name = _value;
    return depositProvider;
  }
  function LARVA_set_depositProvider_post (Provider _depositProvider) LARVA_DEA_1_handle_after_assignment_depositProvider internal returns (Provider) {
    LARVA_previous_depositProvider = depositProvider;
    depositProvider = _depositProvider;
    return depositProvider;
  }
  Provider private LARVA_previous_depositProvider;
  function LARVA_set_history_pre (address _index, string _history_value) LARVA_DEA_1_handle_after_assignment_history internal returns (string) {
    LARVA_previous_history_value = history;
    history[_index] = _history_value;
    return LARVA_previous_history;
  }
  function LARVA_set_history_post (address _index, string _history_value) LARVA_DEA_1_handle_after_assignment_history internal returns (string) {
    LARVA_previous_history_value = history;
    history[_index] = _history_value;
    return history;
  }
  string private LARVA_previous_history;
  function LARVA_transfer (address payable _to, uint amount) LARVA_DEA_1_handle_after_transfer internal {
    LARVA_set_history_pre(_to, amount);
  }
  function LARVA_reparation () private {
    revert();
  }
  enum LARVA_STATUS {RUNNING, STOPPED}
  function LARVA_EnableContract () private {
    LARVA_Status = LARVA_STATUS.RUNNING;
  }
  function LARVA_DisableContract () private {
    LARVA_Status = LARVA_STATUS.STOPPED;
  }
  LARVA_STATUS private LARVA_Status = LARVA_STATUS.STOPPED;
  modifier LARVA_ContractIsEnabled {
    require(LARVA_Status == LARVA_STATUS.RUNNING);
    _;
  }
  uint private deposit;
  address payable owner;
  struct Provider {
    address payable wallet;
    string name;
  }
  Provider private depositProvider;
  mapping (address => string) private history;
  modifier iss (address _owner) {
    require(msg.sender == _owner);
    _;
  }
  constructor (string memory name, address payable _depositProvider) LARVA_Constructor payable {
    require(msg.value > 0, "Must be started with a deposit.");
    (owner, deposit) = ((payable(msg.sender), msg.value));
    depositProvider = Provider(_depositProvider, name);
    history[_depositProvider] = name;
  }
  function transfer (address payable to, uint tokens) LARVA_ContractIsEnabled iss(owner) external returns (bool success) {
    LARVA_transfer(to, tokens);
  }
  function changeOwnerAndDepositProvider (address payable _owner, string memory name, address payable _depositProvider) LARVA_ContractIsEnabled external iss(_depositProvider) returns (bool success) {
    {
      Provider depositProvider_LARVA;
      (owner, depositProvider_LARVA) = ((_owner, Provider(_depositProvider, name)));
      LARVA_set_depositProvider_post(depositProvider_LARVA);
    }
    LARVA_set_history_post(_depositProvider, name);
  }
  function end () LARVA_DEA_1_handle_before_end__parameters_ LARVA_ContractIsEnabled external iss(owner) returns (bool success) {
    LARVA_transfer(depositProvider.wallet, deposit);
    selfdestruct(owner);
  }

}
