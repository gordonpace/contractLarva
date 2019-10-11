pragma solidity ^0.5.11;


contract MultiOwners {
    mapping(address => bool) public owners;
    address[] ownerList;
    uint ownerCount;
    mapping(uint => mapping(address => bool)) actionSignOffs;


    constructor(address[] memory _owners) public{
        for(uint i = 0; i < _owners.length; i++){
            owners[_owners[i]] = true;
        }

        ownerList = _owners;
        ownerCount = ownerList.length;
    }

    modifier anyOwner {
        require(owners[msg.sender]);
        _;
    }

    modifier allOwners (uint _id){
        require(owners[msg.sender]);
        actionSignOffs[_id][msg.sender] = true;

        for(uint i = 0 ; i < ownerList.length; i++){
            if(ownerList[i] != address(0) && !actionSignOffs[_id][ownerList[i]]){
                return;
            }
        }
        _;
    }

    mapping(address => mapping(address => bool)) votesToRemove;
    mapping(address => address[]) votesToRemoveKeys;

    function removeOwner(address _address) anyOwner public{
        votesToRemove[_address][msg.sender] = true;
        votesToRemoveKeys[_address].push(msg.sender);

        uint countInFavour = 0;
        uint totalCount = ownerCount;

        for(uint i = 0; i < totalCount; i++){
            if(votesToRemove[_address][votesToRemoveKeys[_address][i]]){
                countInFavour++;
            }
        }

        uint limit = 2*totalCount/3;

        if(countInFavour >= limit){
            owners[_address] = false;
            ownerCount--;
            for(uint i = 0; i < ownerList.length; i++){
                if(ownerList[i] != address(0) && ownerList[i] == _address){
                    ownerList[i] = address(0);
                }
            }
        }
    }

    mapping(uint => address payable) idTo;
    mapping(uint => uint) idVal;
    uint id;
    
    function proposeTransaction(address payable _to, uint _val) public anyOwner{
        idTo[id] = _to;
        idVal[id] = _val;
        id++;
    }

    function transfer(uint _id) allOwners(_id) public{
        idTo[_id].transfer(idVal[_id]);
    }

    function () external payable {}
}