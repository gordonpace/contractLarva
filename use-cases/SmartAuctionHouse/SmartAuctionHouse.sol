pragma solidity ^0.4.24;

contract SmartAuctionHouse{

    uint currentItem;
    uint startingOffer;

    uint currentOffer;
    address currentWinner;

    uint ticks;

    mapping(uint => address) winners;
    mapping(uint => uint) winningOffer;
    mapping(uint => bool) fulfilled;

    address owner;

    function SmartAuctionHouse(){
        owner = msg.sender;
    }

    modifier onlyOwner(){
        require(msg.sender == owner);
        _;
    }
    
    modifier onlyOwnerOrInternal(){
        require(msg.sender == owner || msg.sender == address(this));
        _;
    }

    function auctionOffItem(uint _offerID, uint _startingOffer) public {
        require(!ongoingAuction());

        currentItem = _offerID;
        startingOffer = _startingOffer;
    }

    function tick() public onlyOwner{
        require(ongoingAuction());

        ticks++;

        if(ticks > 2) declareWinningOffer();
    }

    function makeOffer(uint _offer) public {
        require(_offer > currentOffer);

        currentOffer = _offer;
        currentWinner = msg.sender;
    }

    function declareWinningOffer() public onlyOwnerOrInternal{
        require(ticks > 2);

        winners[currentItem] = currentWinner;
        winningOffer[currentItem] = currentOffer;
        reset();
    }

    function fulfillOffer(uint _id) payable public {
        require(winners[_id] == msg.sender && winningOffer[_id] == msg.value && !fulfilled[_id]);
        fulfilled[_id] = true;
    }

    function ongoingAuction() internal returns(bool){
        return startingOffer == 0 && currentOffer == 0;
    }

    function reset() internal {
        currentItem = 0;
        startingOffer = 0;
        currentOffer = 0;
        currentWinner = address(0);
        ticks = 0;
    }

    function getItemWinningOffer(uint _id) public returns(address,uint){
        return (winners[_id], winningOffer[_id]);
    }

    function getItemWinningBidder(uint _id) public returns(address){
        (address bidder, ) = getItemWinningOffer(_id);
        return bidder;
    }

    function getItemWinningOFfer(uint _id) public returns(uint){
        (, uint winningOffer) = getItemWinningOffer(_id);
        return winningOffer;
    }
}