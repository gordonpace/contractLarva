pragma solidity ^0.4.24;
contract LARVA_SmartAuctionHouse {
  modifier LARVA_DEA_2_handle_after_fulfillOffer__parameters__id (uint _id) {
    if ((LARVA_STATE_2 == 0)) {
      LARVA_STATE_2 = 0;
      unfulfilled[_id] = false;
    }
    _;
  }
  modifier LARVA_DEA_2_handle_after_auctionOffItem__parameters__offerID__startingOffer (uint _offerID, uint _startingOffer) {
    if ((LARVA_STATE_2 == 0) && (areAnyUnfullfilled(msg.sender) && attemptsBeforeFullfillment[msg.sender] < 3)) {
      LARVA_STATE_2 = 1;
      attemptsBeforeFullfillment[msg.sender]++;
      LARVA_reparation();
    } else {
      if ((LARVA_STATE_2 == 0) && (areAnyUnfullfilled(msg.sender) && attemptsBeforeFullfillment[msg.sender] > 3)) {
        LARVA_STATE_2 = 1;
        attemptsBeforeFullfillment[msg.sender]++;
        LARVA_reparation();
      } else {
        if ((LARVA_STATE_2 == 0) && (!areAnyUnfullfilled(msg.sender))) {
          LARVA_STATE_2 = 0;
          attemptsBeforeFullfillment[msg.sender] = 0;
        }
      }
    }
    _;
  }
  modifier LARVA_DEA_2_handle_before_fulfillOffer__parameters__id (uint _id) {
    if ((LARVA_STATE_2 == 0) && (cancelledItems[_id])) {
      LARVA_STATE_2 = 1;
      LARVA_reparation();
    }
    _;
  }
  modifier LARVA_DEA_2_handle_before_declareWinningOffer__parameters_ () {
    if ((LARVA_STATE_2 == 0)) {
      LARVA_STATE_2 = 0;
      unfulfilled[currentItem] = true;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_makeOffer__parameters__offer (uint _offer) {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 1;
      timeSinceLastOffer = now;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_declareWinningOffer__parameters_ () {
    if ((LARVA_STATE_1 == 1)) {
      LARVA_STATE_1 = 0;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_auctionOffItem__parameters__offerID__startingOffer (uint _offerID, uint _startingOffer) {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 1;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_before_auctionOffItem__parameters__offerID__startingOffer (uint _offerID, uint _startingOffer) {
    if ((LARVA_STATE_1 == 1) && (now - timeSinceLastOffer < 15 minutes)) {
      LARVA_STATE_1 = 2;
      LARVA_reparation();
    } else {
      if ((LARVA_STATE_1 == 1) && (now - timeSinceLastOffer >= 15 minutes)) {
        LARVA_STATE_1 = 1;
        forceDeclareWinner();
      }
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  int8 LARVA_STATE_2 = 0;
  function LARVA_reparation () private {
    revert();
  }
  mapping (address => uint) attemptsBeforeFullfillment;
  mapping (uint => bool) cancelledItems;
  mapping (uint => bool) unfulfilled;
  mapping (address => uint[]) wonBids;
  uint timeSinceLastOffer;
  function forceDeclareWinner () private {
    ticks = 3;
    declareWinningOffer();
    timeSinceLastOffer = 0;
  }
  function cancelAnyUnfulfilledBids (address _bidder) private {
    for (uint i = wonBids[_bidder].length - 1; i >= 0; (i)--) {
      if (unfulfilled[wonBids[_bidder][i]]) {
        cancelledItems[wonBids[_bidder][i]] = false;
      }
    }
  }
  function areAnyUnfullfilled (address _bidder) private returns (bool) {
    for (uint i = wonBids[_bidder].length - 1; i >= 0; (i)--) {
      if (unfulfilled[wonBids[_bidder][i]]) {
        return true;
      }
    }
    return false;
  }
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
  uint currentItem;
  uint startingOffer;
  uint currentOffer;
  address currentWinner;
  uint ticks;
  mapping (uint => address) winners;
  mapping (uint => uint) winningOffer;
  mapping (uint => bool) fulfilled;
  address owner;
  function LARVA_SmartAuctionHouse () LARVA_Constructor LARVA_ContractIsEnabled {
    owner = msg.sender;
  }
  modifier onlyOwner () {
    require(msg.sender == owner);
    _;
  }
  modifier onlyOwnerOrInternal () {
    require(msg.sender == owner || msg.sender == address(this));
    _;
  }
  function auctionOffItem (uint _offerID, uint _startingOffer) LARVA_DEA_2_handle_after_auctionOffItem__parameters__offerID__startingOffer(_offerID, _startingOffer) LARVA_DEA_1_handle_after_auctionOffItem__parameters__offerID__startingOffer(_offerID, _startingOffer) LARVA_DEA_1_handle_before_auctionOffItem__parameters__offerID__startingOffer(_offerID, _startingOffer) LARVA_ContractIsEnabled public {
    require(!ongoingAuction());
    currentItem = _offerID;
    startingOffer = _startingOffer;
  }
  function tick () LARVA_ContractIsEnabled public onlyOwner {
    require(ongoingAuction());
    ticks++;
    if (ticks > 2) declareWinningOffer();
  }
  function makeOffer (uint _offer) LARVA_DEA_1_handle_after_makeOffer__parameters__offer(_offer) LARVA_ContractIsEnabled public {
    require(_offer > currentOffer);
    currentOffer = _offer;
    currentWinner = msg.sender;
  }
  function declareWinningOffer () LARVA_DEA_2_handle_before_declareWinningOffer__parameters_ LARVA_DEA_1_handle_after_declareWinningOffer__parameters_ LARVA_ContractIsEnabled public onlyOwnerOrInternal {
    require(ticks > 2);
    winners[currentItem] = currentWinner;
    winningOffer[currentItem] = currentOffer;
    reset();
  }
  function fulfillOffer (uint _id) LARVA_DEA_2_handle_after_fulfillOffer__parameters__id(_id) LARVA_DEA_2_handle_before_fulfillOffer__parameters__id(_id) LARVA_ContractIsEnabled payable public {
    require(winners[_id] == msg.sender && winningOffer[_id] == msg.value && !fulfilled[_id]);
    fulfilled[_id] = true;
  }
  function ongoingAuction () LARVA_ContractIsEnabled internal returns (bool) {
    return startingOffer == 0 && currentOffer == 0;
  }
  function reset () LARVA_ContractIsEnabled internal {
    currentItem = 0;
    startingOffer = 0;
    currentOffer = 0;
    currentWinner = address(0);
    ticks = 0;
  }
  function getItemWinningOffer (uint _id) LARVA_ContractIsEnabled public returns (address, uint) {
    return (winners[_id], winningOffer[_id]);
  }
  function getItemWinningBidder (uint _id) LARVA_ContractIsEnabled public returns (address) {
    (address bidder, ) = (getItemWinningOffer(_id));
    return bidder;
  }
  function getItemWinningOFfer (uint _id) LARVA_ContractIsEnabled public returns (uint) {
    (, uint winningOffer) = (getItemWinningOffer(_id));
    return winningOffer;
  }

}
