  contract InsuredCourierService{
      bool ordered;
      bool delivered;
      uint value = 1 ether;
      address buyer;
      
      function order(uint _eta, address _buyer, string memory _address) payable public{
          require(!ordered && msg.value == value);
          ordered = true;
          buyer = _buyer;
      }
      
      function deliver(address _signer, string memory _address) public{
          require(!delivered);
          delivered = true;
      }
  
      function complain() public{
          require(msg.sender == buyer && !delivered);
      }
  }