pragma solidity ^0.4.15;


contract Procurement {
    enum ContractStatus { Proposed, Open, Closed }
    enum OrderStatus { Ordered, Delivered }

    struct Order {
        bool exists;
        uint cost;
        OrderStatus status;
        uint deliveryTimeDue;
    }

    // Addresses of buyer and seller in contract
    address public seller;
    address public buyer;
    
    // Contract parameters
    uint public minimumItemsToBeOrdered;
    uint public maximumItemsToBeOrdered;
    uint public costPerUnit;
    uint public performanceGuarantee;
    uint public endOfContractTimestamp;
    
    // Contract status
    ContractStatus public contractStatus;
    uint8 public itemsOrderedCount;
    uint public moneyLeftInContract;

    // Orders
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

    function Procurement(
        address _seller, address _buyer,
        uint _minimumItemsToBeOrdered, uint _maximumItemsToBeOrdered,
        uint _costPerUnit,
        uint _performanceGuarantee,
        uint _contractDuration
    ) public payable
    {
        // The minimum order size must be put in escrow at signing time
        require(msg.value >= _costPerUnit * _minimumItemsToBeOrdered);

        // Set the contract parameters
        seller = _seller;
        buyer = _buyer;

        minimumItemsToBeOrdered = _minimumItemsToBeOrdered;
        maximumItemsToBeOrdered = _maximumItemsToBeOrdered;

        costPerUnit = _costPerUnit;

        endOfContractTimestamp = now + _contractDuration;

        performanceGuarantee = _performanceGuarantee;

        // Contract status
        contractStatus = ContractStatus.Proposed;
        itemsOrderedCount = 0;
        moneyLeftInContract = msg.value;

        // Initialise orders
        pendingOrderCount = 0;
        pendingOrderCost = 0;
        lateOrdersCount = 0;
        inTimeOrdersCount = 0;
        
    }

    function acceptContract() public payable bySeller {
        require(msg.value >= performanceGuarantee);
        contractStatus = ContractStatus.Open;
    }

    function createOrder(
        uint8 _orderNumber,
        uint8 _orderSize,
        uint _orderDeliveryTimeLeft
    ) public payable byBuyer 
    {
        // Order does not already exist
        require(!orders[_orderNumber].exists);
        // Number of items ordered does not exceed maximum
        require(itemsOrderedCount + _orderSize <= maximumItemsToBeOrdered);
        // Order delivery deadline will not be too late
        require(now + _orderDeliveryTimeLeft <= endOfContractTimestamp);

        // Ensure there is enough money left in the contract to pay for the order
        uint orderCost = _orderSize * costPerUnit;
        moneyLeftInContract += msg.value;
        require(orderCost <= moneyLeftInContract);
        moneyLeftInContract -= orderCost;
        
        // Update number of items ordered
        itemsOrderedCount += _orderSize;

        // Update contract status
        pendingOrderCount++;
        pendingOrderCost += orderCost;

        // Record the order
        orders[_orderNumber] = Order(true, orderCost, OrderStatus.Ordered, now+_orderDeliveryTimeLeft);
    }

    function deliveryMade(
        uint8 _orderNumber
    ) public byBuyer 
    {
        Order memory order = orders[_orderNumber];

        // Ensure that the order exists and has not yet been delivered
        require(order.exists && order.status == OrderStatus.Ordered);

        // Order state update
        order.status = OrderStatus.Delivered;

        // Contract state update
        if (order.deliveryTimeDue < now) {
            lateOrdersCount++;
        } else {
            inTimeOrdersCount++;
        }

        pendingOrderCount--;
        pendingOrderCost -= order.cost;

        // Pay the seller
        seller.transfer(order.cost);
    }

    function terminateContract() public {
        // Can only be done by the seller or buyer
        require(msg.sender == seller || msg.sender == buyer);

        // Can only be closed after the contract time frame ended
        require(now > endOfContractTimestamp);

        if (msg.sender == seller) {
            // Can only be closed by seller if there are no pending orders
            require(pendingOrderCount == 0);
        }

        if (pendingOrderCount > 0) {
            // If there are any undelivered orders, return their cost to the buyer 
            buyer.transfer(pendingOrderCost);
        } else {
            // If there are no undelivered orders, and not enough orders were made (less 
            // than minimum) the seller gets money for the unordered items
            if (itemsOrderedCount < minimumItemsToBeOrdered) {
                seller.transfer((itemsOrderedCount-minimumItemsToBeOrdered)*costPerUnit);
            }
        }

        // If there are any pending orders or 25%+ of the orders were delivered late
        // the buyer gets the performance guarantee, otherwise it is returned to the seller
        if ((pendingOrderCount > 0) || (lateOrdersCount * 3 >= inTimeOrdersCount)) {
            buyer.transfer(performanceGuarantee);
        } else {
            seller.transfer(performanceGuarantee);
        }

    }

}