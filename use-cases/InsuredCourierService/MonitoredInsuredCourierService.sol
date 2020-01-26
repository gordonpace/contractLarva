contract LARVA_InsuredCourierService {
  modifier LARVA_DEA_1_handle_after_order__no_parameters {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 1;
      orderedTime = now;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_deliver__no_parameters {
    if ((LARVA_STATE_1 == 1) && (now - orderedTime <= minimumInsuredDeliveryTime)) {
      LARVA_STATE_1 = 2;
      LARVA_satisfaction();
    } else {
      if ((LARVA_STATE_1 == 1) && (now - orderedTime >= minimumInsuredDeliveryTime)) {
        LARVA_STATE_1 = 3;
        LARVA_reparation();
      }
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  function LARVA_reparation () private {
    getInsured().transfer(getStake());
    LARVA_DisableContract();
  }
  function LARVA_satisfaction () private {
    getInsurer().transfer(getStake());
  }
  uint orderedTime;
  uint minimumInsuredDeliveryTime = 24 * 30 hours;
  address payable private insurer_address;
  function getStake () private returns (uint) {
    return value;
  }
  function getInsurer () private returns (address payable) {
    return insurer_address;
  }
  function getInsured () private returns (address payable) {
    return customer;
  }
  enum STAKE_STATUS {UNPAID, PAID}
  STAKE_STATUS private stake_status = STAKE_STATUS.UNPAID;
  function payStake () payable public {
    require(stake_status == STAKE_STATUS.UNPAID);
    require(msg.value == getStake());
    require(msg.sender == getInsurer());
    stake_status = STAKE_STATUS.PAID;
    LARVA_EnableContract();
  }
  constructor () public {
    insurer_address = msg.sender;
  }
  modifier LARVA_Constructor {
    require(LARVA_Status == LARVA_STATUS.READY);
    LARVA_Status = LARVA_STATUS.RUNNING;
    _;
  }
  function InsuredCourierServiceConstructor () LARVA_Constructor public {
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
  bool ordered;
  bool delivered;
  uint value = 1 ether;
  address buyer;
  function order (uint _eta, address _buyer, string memory _address) LARVA_DEA_1_handle_after_order__no_parameters LARVA_ContractIsEnabled public {
    require(!ordered && msg.value == value);
    ordered = true;
    buyer = _buyer;
  }
  function deliver (address _signer, string memory _address) LARVA_DEA_1_handle_after_deliver__no_parameters LARVA_ContractIsEnabled public {
    require(!delivered);
    delivered = true;
  }
  function complain () LARVA_ContractIsEnabled public {
    require(msg.sender == buyer && !delivered);
  }

}
