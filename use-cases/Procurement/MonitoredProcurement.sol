pragma solidity ^0.4.15;
contract LARVA_Procurement {
  modifier LARVA_DEA_2_handle_after_assignment_minimumItemsToBeOrdered {
    _;
    if ((LARVA_STATE_2 == 0) && (LARVA_previous_minimumItemsToBeOrdered != minimumItemsToBeOrdered)) {
      LARVA_STATE_2 = 1;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_2_handle_after_assignment_maximumItemsToBeOrdered {
    _;
    if ((LARVA_STATE_2 == 0) && (LARVA_previous_minimumItemsToBeOrdered != minimumItemsToBeOrdered)) {
      LARVA_STATE_2 = 1;
      LARVA_reparation();
    }
  }
  modifier LARVA_DEA_2_handle_after_terminateContract__parameters_ () {
    if ((LARVA_STATE_2 == 0) && (orderCount < minimumItemsToBeOrdered || orderCount > maximumItemsToBeOrdered)) {
      LARVA_STATE_2 = 2;
      LARVA_reparation();
    } else {
      if ((LARVA_STATE_2 == 0) && (orderCount >= minimumItemsToBeOrdered && orderCount <= maximumItemsToBeOrdered)) {
        LARVA_STATE_2 = 3;
      }
    }
    _;
  }
  modifier LARVA_DEA_2_handle_after_createOrder__parameters__orderNumber__orderSize__orderDeliveryTimeLeft (uint8 _orderNumber, uint8 _orderSize, uint _orderDeliveryTimeLeft) {
    if ((LARVA_STATE_2 == 0)) {
      LARVA_STATE_2 = 0;
      orderCount += _orderNumber;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_terminateContract__no_parameters {
    if ((LARVA_STATE_1 == 0) && (this.balance != 0)) {
      LARVA_STATE_1 = 1;
      enoughItemsOrderedBadStateReached = true;
      LARVA_reparation();
    } else {
      if ((LARVA_STATE_1 == 0) && (this.balance == 0)) {
        LARVA_STATE_1 = 2;
      }
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  int8 LARVA_STATE_2 = 0;
  function LARVA_set_minimumItemsToBeOrdered_pre (uint _minimumItemsToBeOrdered) LARVA_DEA_2_handle_after_assignment_minimumItemsToBeOrdered public returns (uint) {
    LARVA_previous_minimumItemsToBeOrdered = minimumItemsToBeOrdered;
    minimumItemsToBeOrdered = _minimumItemsToBeOrdered;
    return LARVA_previous_minimumItemsToBeOrdered;
  }
  function LARVA_set_minimumItemsToBeOrdered_post (uint _minimumItemsToBeOrdered) LARVA_DEA_2_handle_after_assignment_minimumItemsToBeOrdered public returns (uint) {
    LARVA_previous_minimumItemsToBeOrdered = minimumItemsToBeOrdered;
    minimumItemsToBeOrdered = _minimumItemsToBeOrdered;
    return minimumItemsToBeOrdered;
  }
  uint private LARVA_previous_minimumItemsToBeOrdered;
  function LARVA_set_maximumItemsToBeOrdered_pre (uint _maximumItemsToBeOrdered) LARVA_DEA_2_handle_after_assignment_maximumItemsToBeOrdered public returns (uint) {
    LARVA_previous_maximumItemsToBeOrdered = maximumItemsToBeOrdered;
    maximumItemsToBeOrdered = _maximumItemsToBeOrdered;
    return LARVA_previous_maximumItemsToBeOrdered;
  }
  function LARVA_set_maximumItemsToBeOrdered_post (uint _maximumItemsToBeOrdered) LARVA_DEA_2_handle_after_assignment_maximumItemsToBeOrdered public returns (uint) {
    LARVA_previous_maximumItemsToBeOrdered = maximumItemsToBeOrdered;
    maximumItemsToBeOrdered = _maximumItemsToBeOrdered;
    return maximumItemsToBeOrdered;
  }
  uint private LARVA_previous_maximumItemsToBeOrdered;
  function LARVA_reparation () private {
    enoughItemsOrderedBadStateReached?mediator.transfer(this.balance):();
  }
  address mediator;
  uint orderCount;
  bool enoughItemsOrderedBadStateReached;
  modifier LARVA_Constructor {
    {
      mediator = address(uint160(msg.sender));
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
  enum ContractStatus {Proposed, Open, Closed}
  enum OrderStatus {Ordered, Delivered}
  struct Order {
    bool exists;
    uint cost;
    OrderStatus status;
    uint deliveryTimeDue;
  }
  address public seller;
  address public buyer;
  uint private minimumItemsToBeOrdered;
  uint private maximumItemsToBeOrdered;
  uint public costPerUnit;
  uint public performanceGuarantee;
  uint public endOfContractTimestamp;
  ContractStatus public contractStatus;
  uint8 public itemsOrderedCount;
  uint public moneyLeftInContract;
  mapping (uint8 => Order) public orders;
  uint8 public pendingOrderCount;
  uint public pendingOrderCost;
  uint public lateOrdersCount;
  uint public inTimeOrdersCount;
  modifier bySeller {
    require(msg.sender == seller);
    _;
  }
  modifier byBuyer {
    require(msg.sender == buyer);
    _;
  }
  function LARVA_Procurement (address _seller, address _buyer, uint _minimumItemsToBeOrdered, uint _maximumItemsToBeOrdered, uint _costPerUnit, uint _performanceGuarantee, uint _contractDuration) LARVA_Constructor LARVA_ContractIsEnabled public payable {
    require(msg.value >= _costPerUnit * _minimumItemsToBeOrdered);
    seller = _seller;
    buyer = _buyer;
    minimumItemsToBeOrdered = _minimumItemsToBeOrdered;
    maximumItemsToBeOrdered = _maximumItemsToBeOrdered;
    costPerUnit = _costPerUnit;
    endOfContractTimestamp = now + _contractDuration;
    performanceGuarantee = _performanceGuarantee;
    contractStatus = ContractStatus.Proposed;
    itemsOrderedCount = 0;
    moneyLeftInContract = msg.value;
    pendingOrderCount = 0;
    pendingOrderCost = 0;
    lateOrdersCount = 0;
    inTimeOrdersCount = 0;
  }
  function acceptContract () LARVA_ContractIsEnabled public payable bySeller {
    require(msg.value >= performanceGuarantee);
    contractStatus = ContractStatus.Open;
  }
  function createOrder (uint8 _orderNumber, uint8 _orderSize, uint _orderDeliveryTimeLeft) LARVA_DEA_2_handle_after_createOrder__parameters__orderNumber__orderSize__orderDeliveryTimeLeft(_orderNumber, _orderSize, _orderDeliveryTimeLeft) LARVA_ContractIsEnabled public payable byBuyer {
    require(!orders[_orderNumber].exists);
    require(itemsOrderedCount + _orderSize <= maximumItemsToBeOrdered);
    require(now + _orderDeliveryTimeLeft <= endOfContractTimestamp);
    uint orderCost = _orderSize * costPerUnit;
    moneyLeftInContract += msg.value;
    require(orderCost <= moneyLeftInContract);
    moneyLeftInContract -= orderCost;
    itemsOrderedCount += _orderSize;
    pendingOrderCount++;
    pendingOrderCost += orderCost;
    orders[_orderNumber] = Order(true, orderCost, OrderStatus.Ordered, now + _orderDeliveryTimeLeft);
  }
  function deliveryMade (uint8 _orderNumber) LARVA_ContractIsEnabled public byBuyer {
    Order memory order = orders[_orderNumber];
    require(order.exists && order.status == OrderStatus.Ordered);
    order.status = OrderStatus.Delivered;
    if (order.deliveryTimeDue < now) {
      lateOrdersCount++;
    } else {
      inTimeOrdersCount++;
    }
    (pendingOrderCount)--;
    pendingOrderCost -= order.cost;
    seller.transfer(order.cost);
  }
  function terminateContract () LARVA_DEA_2_handle_after_terminateContract__parameters_ LARVA_DEA_1_handle_after_terminateContract__no_parameters LARVA_ContractIsEnabled public {
    require(msg.sender == seller || msg.sender == buyer);
    require(now > endOfContractTimestamp);
    if (msg.sender == seller) {
      require(pendingOrderCount == 0);
    }
    if (pendingOrderCount > 0) {
      buyer.transfer(pendingOrderCost);
    } else {
      if (itemsOrderedCount < minimumItemsToBeOrdered) {
        seller.transfer((itemsOrderedCount - minimumItemsToBeOrdered) * costPerUnit);
      }
    }
    if ((pendingOrderCount > 0) || (lateOrdersCount * 3 >= inTimeOrdersCount)) {
      buyer.transfer(performanceGuarantee);
    } else {
      seller.transfer(performanceGuarantee);
    }
  }

}
