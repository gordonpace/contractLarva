contract CourierService{

    uint fees;

    address payable customer;
    uint orderETA;
    uint orderDeliveryTime;
    bool delivered;
    bool ordered;

    bool cancelled;

    uint extraTimeAllowance;
    uint cost;

    function order(uint eta) payable public{
        require(msg.value >= cost);
        require(!ordered);

        customer = msg.sender;
        orderETA = eta;
        ordered = true;
    }

    function delivery() public{
        require(msg.sender == customer);
        require(ordered && !delivered);

        delivered = true;

        orderDeliveryTime = now;
    }

    function requestRefund() public payable{
        require(ordered && !delivered && !cancelled);
        require(now - orderETA > 5 days);

        transferTo(cost);
        cancelled = true;
    }

    function transferTo(uint val) public payable{
        require(msg.sender == address(this));

        customer.call.value(val);
    }
}
