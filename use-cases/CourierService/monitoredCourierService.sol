contract LARVA_CourierService {
  modifier LARVA_DEA_1_handle_after_refund__parameters_ () {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 6;
    } else {
      if ((LARVA_STATE_1 == 2)) {
        LARVA_STATE_1 = 6;
      } else {
        if ((LARVA_STATE_1 == 3)) {
          LARVA_STATE_1 = 6;
        } else {
          if ((LARVA_STATE_1 == 4)) {
            LARVA_STATE_1 = 5;
          } else {
            if ((LARVA_STATE_1 == 5)) {
              LARVA_STATE_1 = 6;
            }
          }
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_order__parameters_eta (uint eta) {
    if ((LARVA_STATE_1 == 0) && (customer != msg.sender)) {
      LARVA_STATE_1 = 6;
    } else {
      if ((LARVA_STATE_1 == 0) && (customer == msg.sender)) {
        LARVA_STATE_1 = 1;
      } else {
        if ((LARVA_STATE_1 == 1)) {
          LARVA_STATE_1 = 6;
        } else {
          if ((LARVA_STATE_1 == 2)) {
            LARVA_STATE_1 = 6;
          } else {
            if ((LARVA_STATE_1 == 3)) {
              LARVA_STATE_1 = 6;
            } else {
              if ((LARVA_STATE_1 == 4)) {
                LARVA_STATE_1 = 6;
              } else {
                if ((LARVA_STATE_1 == 5)) {
                  LARVA_STATE_1 = 6;
                }
              }
            }
          }
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_delivery__parameters_ () {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 6;
    } else {
      if ((LARVA_STATE_1 == 1) && (customer != msg.sender)) {
        LARVA_STATE_1 = 6;
      } else {
        if ((LARVA_STATE_1 == 1) && (customer == msg.sender)) {
          LARVA_STATE_1 = 2;
        } else {
          if ((LARVA_STATE_1 == 2)) {
            LARVA_STATE_1 = 6;
          } else {
            if ((LARVA_STATE_1 == 3)) {
              LARVA_STATE_1 = 6;
            } else {
              if ((LARVA_STATE_1 == 4)) {
                LARVA_STATE_1 = 6;
              } else {
                if ((LARVA_STATE_1 == 5)) {
                  LARVA_STATE_1 = 6;
                }
              }
            }
          }
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_1_handle_before_transferTo__parameters_val (uint val) {
    if ((LARVA_STATE_1 == 3)) {
      LARVA_STATE_1 = 4;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_before_refund__parameters_ () {
    if ((LARVA_STATE_1 == 1) && (customer != msg.sender)) {
      LARVA_STATE_1 = 6;
    } else {
      if ((LARVA_STATE_1 == 1) && (msg.sender == customer)) {
        LARVA_STATE_1 = 3;
      }
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  modifier LARVA_Constructor {
    {
    }
    LARVA_Status = LARVA_STATUS.RUNNING;
    _;
  }
  constructor () LARVA_Constructor public {
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
  uint fees;
  address customer;
  uint orderETA;
  uint orderDeliveryTime;
  bool delivered;
  bool ordered;
  bool cancelled;
  uint extraTimeAllowance;
  uint cost;
  function order (uint eta) LARVA_DEA_1_handle_after_order__parameters_eta(eta) LARVA_ContractIsEnabled payable public {
    require(msg.value >= cost);
    require(!ordered);
    customer = msg.sender;
    orderETA = eta;
    ordered = true;
  }
  function delivery () LARVA_DEA_1_handle_after_delivery__parameters_ LARVA_ContractIsEnabled public {
    require(msg.sender == customer);
    require(ordered && !delivered);
    delivered = true;
    orderDeliveryTime = now;
  }
  function requestRefund () LARVA_ContractIsEnabled public payable {
    require(ordered && !delivered && !cancelled);
    require(now - orderETA > 5 days);
    transferTo(cost);
    cancelled = true;
  }
  function transferTo (uint val) LARVA_DEA_1_handle_before_transferTo__parameters_val(val) LARVA_ContractIsEnabled public payable {
    require(msg.sender == address(this));
    customer.call.value(val);
  }

}
